// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.dontTouch
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import chisel3.experimental.IntParam
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter

case object BuildRoCC extends Field[Seq[Parameters => LazyRoCC]](Nil)

class RoCCInstruction extends Bundle {
  val funct = Bits(7.W)
  val rs2 = Bits(5.W)
  val rs1 = Bits(5.W)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val rd = Bits(5.W)
  val opcode = Bits(7.W)
}

class RoCCCommand(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = new RoCCInstruction
  val rs1 = Bits(xLen.W)
  val rs2 = Bits(xLen.W)
  val status = new MStatus
}

class RoCCResponse(implicit p: Parameters) extends CoreBundle()(p) {
  val rd = Bits(5.W)
  val data = Bits(xLen.W)
}

class RoCCCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val cmd = Flipped(Decoupled(new RoCCCommand))
  val resp = Decoupled(new RoCCResponse)
  val mem = new HellaCacheIO
  val busy = Output(Bool())
  val interrupt = Output(Bool())
  val exception = Input(Bool())
}

class RoCCIO(val nPTWPorts: Int)(implicit p: Parameters) extends RoCCCoreIO()(p) {
  val ptw = Vec(nPTWPorts, new TLBPTWIO)
  val fpu_req = Decoupled(new FPInput)
  val fpu_resp = Flipped(Decoupled(new FPResult))
}

/** Base classes for Diplomatic TL2 RoCC units **/
abstract class LazyRoCC(
      val opcodes: OpcodeSet,
      val nPTWPorts: Int = 0,
      val usesFPU: Boolean = false
    )(implicit p: Parameters) extends LazyModule {
  val module: LazyRoCCModuleImp
  val atlNode: TLNode = TLIdentityNode()
  val tlNode: TLNode = TLIdentityNode()
}

class LazyRoCCModuleImp(outer: LazyRoCC) extends LazyModuleImp(outer) {
  val io = IO(new RoCCIO(outer.nPTWPorts))
}

/** Mixins for including RoCC **/

trait HasLazyRoCC extends CanHavePTW { this: BaseTile =>
  val roccs = p(BuildRoCC).map(_(p))

  roccs.map(_.atlNode).foreach { atl => tlMasterXbar.node :=* atl }
  roccs.map(_.tlNode).foreach { tl => tlOtherMastersNode :=* tl }

  nPTWPorts += roccs.map(_.nPTWPorts).sum
  nDCachePorts += roccs.size
}

trait HasLazyRoCCModule extends CanHavePTWModule
    with HasCoreParameters { this: RocketTileModuleImp with HasFpuOpt =>

  val (respArb, cmdRouter) = if(outer.roccs.nonEmpty) {
    val respArb = Module(new RRArbiter(new RoCCResponse()(outer.p), outer.roccs.size))
    val cmdRouter = Module(new RoccCommandRouter(outer.roccs.map(_.opcodes))(outer.p))
    outer.roccs.zipWithIndex.foreach { case (rocc, i) =>
      rocc.module.io.ptw ++=: ptwPorts
      rocc.module.io.cmd <> cmdRouter.io.out(i)
      val dcIF = Module(new SimpleHellaCacheIF()(outer.p))
      dcIF.io.requestor <> rocc.module.io.mem
      dcachePorts += dcIF.io.cache
      respArb.io.in(i) <> Queue(rocc.module.io.resp)
    }

    fpuOpt foreach { fpu =>
      val nFPUPorts = outer.roccs.count(_.usesFPU)
      println("****----nFPUPorts----****")
      println(nFPUPorts)
      if (usingFPU && nFPUPorts > 0) {
        val fpArb = Module(new InOrderArbiter(new FPInput()(outer.p), new FPResult()(outer.p), nFPUPorts))
        val fp_rocc_ios = outer.roccs.filter(_.usesFPU).map(_.module.io)
        fpArb.io.in_req <> fp_rocc_ios.map(_.fpu_req)
        fp_rocc_ios.zip(fpArb.io.in_resp).foreach {
          case (rocc, arb) => rocc.fpu_resp <> arb
        }
        fpu.io.cp_req <> fpArb.io.out_req
        fpArb.io.out_resp <> fpu.io.cp_resp
      } else {
        fpu.io.cp_req.valid := false.B
        fpu.io.cp_resp.ready := false.B
      }
    }
    (Some(respArb), Some(cmdRouter))
  } else {
    (None, None)
  }
}
/*
class FPInput(implicit p: Parameters) extends CoreBundle()(p) with HasFPUCtrlSigs {
  val rm = Bits(width = FPConstants.RM_SZ)
  val fmaCmd = Bits(width = 2)
  val typ = Bits(width = 2)
  val fmt = Bits(width = 2)
  val in1 = Bits(width = fLen+1)
  val in2 = Bits(width = fLen+1)
  val in3 = Bits(width = fLen+1)

  override def cloneType = new FPInput().asInstanceOf[this.type]
}
class FPResult(implicit p: Parameters) extends CoreBundle()(p) {
  val data = Bits(width = fLen+1)
  val exc = Bits(width = FPConstants.FLAGS_SZ)
}
*/
class AccumulatorExample(opcodes: OpcodeSet, val n: Int = 16)(implicit p: Parameters) extends LazyRoCC(opcodes,usesFPU=true) {
  override lazy val module = new AccumulatorExampleModuleImp(this)
}

class AccumulatorExampleModuleImp(outer: AccumulatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  val regfile = Mem(outer.n, UInt(xLen.W))
  val busy = RegInit(VecInit(Seq.fill(outer.n){false.B}))
  val cmd = Queue(io.cmd,16)
  val posit_to_int  = Module( new posit_to_int(32,64))
  val Int_to_Posit  = Module( new Int_to_Posit(32,2))
  val Posit_to_Float = Module (new posit_to_float(32,2))
  val FP_to_posit = Module(new FP_to_posit(32,8,2)) 
  val posit_add_module = Module(new posit_add(32,2))
  val posit_add_chisel_module = Module(new PositAdd(32,2))
  val posit_mult_module = Module(new posit_mult(32,2))
  val bct_posit_div_module = Module(new bct_posit_div(32,2))
  val funct = cmd.bits.inst.funct
  val addr = cmd.bits.rs2(8-1,0)
  val addr_reg = RegNext(addr)
  val cmd_fire_reg = RegNext(cmd.fire())
  val convert_int_to_posit = funct === 0.U & cmd.fire()
  val convert_int_to_posit_reg = RegInit(false.B)
  val convert_posit_to_int = funct === 1.U & cmd.fire()//
  val convert_posit_to_int_reg = RegInit(false.B)
  val convert_float_to_posit = funct === 2.U & cmd.fire()//
  val convert_float_to_posit_reg = RegInit(false.B)
  val convert_posit_to_float = funct === 3.U & cmd.fire()//
  val convert_posit_to_float_reg = RegInit(false.B)
  val posit_write = funct === 4.U & cmd.fire()
  val posit_write_reg = RegInit(false.B)
  val posit_add = funct === 5.U & cmd.fire()
  val posit_add_reg = RegInit(false.B)
  val posit_sub = funct === 6.U & cmd.fire()
  val posit_sub_reg = RegInit(false.B)
  val posit_mul = funct === 7.U & cmd.fire()
  val posit_mul_reg = RegInit(false.B)
  val posit_div = funct === 8.U & cmd.fire()
  val posit_div_reg = RegInit(false.B)
  val posit_sqrt = funct === 9.U & cmd.fire() 
  convert_int_to_posit_reg := convert_int_to_posit 
  convert_posit_to_int_reg := convert_posit_to_int 
  convert_float_to_posit_reg := convert_float_to_posit 
  convert_posit_to_float_reg := convert_posit_to_float 
  posit_write_reg := posit_write 
  posit_add_reg := posit_add 
  posit_sub_reg := posit_sub 
  posit_mul_reg := posit_mul 
  posit_div_reg := posit_div 
  val convert_posit_to_float_out = RegInit(0.U(32.W))
  val convert_posit_to_float_dest = RegInit(0.U(5.W))
  val convert_posit_to_float_write = RegInit(false.B) 
  val delayed_posit_to_float_in = RegInit(0.U(32.W))
  val delayed_convert_posit_to_float_dest = RegInit(0.U(5.W))
  val delayed_convert_posit_to_float = RegInit(false.B)
  val block_convert = WireInit(false.B)
  block_convert:=convert_posit_to_float_write & !io.fpu_req.ready & convert_posit_to_float
  val float_to_posit_delayed_rs1 = RegInit(0.U(32.W))
  val float_to_posit_delayed_rs2 = RegInit(0.U(32.W))
  val float_to_posit_delayed = RegInit(false.B)
  val get_fpu_reg = RegInit(false.B)
  val float_to_posit_dest = RegInit(0.U(2.W))
  val delayed_convert_posit_to_float_reg = RegInit(false.B)
  val io_fpu_resp_fire = io.fpu_resp.valid
  io.fpu_req.bits.rm:=0.U
  val fpu_req_1 = Wire(Bool())
  val fpu_req_2 = Wire(Bool())
  val fpu_req_in1_1 = Wire(UInt(65.W))
  val fpu_req_in1_2 = Wire(UInt(65.W))
  val fpu_req_in2_1 = Wire(UInt(65.W))
  val fpu_req_in2_2 = Wire(UInt(65.W))
  val fpu_req_fmaCmd_1 = Wire(UInt(2.W))
  val fpu_req_fmaCmd_2 = Wire(UInt(2.W))
  val block_cmd_1 = RegInit(false.B)
  val block_cmd_5 = RegInit(false.B)
  when(convert_posit_to_float & !delayed_convert_posit_to_float_reg)
  {
    Posit_to_Float.io.in:=regfile(cmd.bits.rs1)
    convert_posit_to_float_out:=Posit_to_Float.io.out
    convert_posit_to_float_dest:=cmd.bits.rs2
    fpu_req_1:=false.B
    fpu_req_fmaCmd_1:=0.U 
    fpu_req_in1_1:=0.U 
    fpu_req_in2_1:= 0.U 
    block_cmd_1:=true.B
  }.elsewhen(convert_posit_to_float_reg | delayed_convert_posit_to_float_reg)
  {    
    when(io.fpu_req.ready)
    {
      fpu_req_1:=true.B
      fpu_req_fmaCmd_1:=1.U 
      fpu_req_in1_1:=convert_posit_to_float_dest 
      fpu_req_in2_1:= convert_posit_to_float_out 
      delayed_convert_posit_to_float_reg:=false.B
      block_cmd_1:=false.B
    }.otherwise
    {
      fpu_req_1:=false.B
      fpu_req_fmaCmd_1:=0.U 
      fpu_req_in1_1:=0.U 
      fpu_req_in2_1:= 0.U 
      delayed_convert_posit_to_float_reg:=true.B
    }
  }.otherwise
  {
    fpu_req_1:=false.B
    fpu_req_fmaCmd_1:=0.U 
    fpu_req_in1_1:=0.U
    fpu_req_in2_1:= 0.U 
  }
  val float_to_posit_reg_val = RegInit(0.U(32.W)) 
  val fpu_resp_valid_reg = RegNext(io.fpu_resp.valid)
  val fpu_resp_bits_exc = RegNext(io.fpu_resp.bits.exc)

  when(convert_float_to_posit | float_to_posit_delayed)
  {
    block_cmd_5:=true.B
    FP_to_posit.io.in:=0.U
    when(io.fpu_req.ready){
      fpu_req_2:=true.B
      fpu_req_fmaCmd_2:=2.U 
      fpu_req_in1_2:=Mux(float_to_posit_delayed,float_to_posit_delayed_rs1,cmd.bits.rs1) 
      float_to_posit_delayed_rs1:=0.U
      float_to_posit_delayed:=false.B
      get_fpu_reg:=true.B
    }.otherwise
    {
      fpu_req_2:=false.B
      fpu_req_fmaCmd_2:=0.U 
      fpu_req_in1_2:=0.U
      float_to_posit_delayed:=true.B
      get_fpu_reg:=false.B
      when(!float_to_posit_delayed)
      {
        float_to_posit_delayed_rs1:=cmd.bits.rs1 
      }
    }
    when(!float_to_posit_delayed) 
    {
      float_to_posit_dest:=addr
    }
  }.elsewhen(io.fpu_resp.valid)
  {
    block_cmd_5:=false.B
    FP_to_posit.io.in:=io.fpu_resp.bits.data
    fpu_req_2:=false.B
    fpu_req_fmaCmd_2:=0.U 
    fpu_req_in1_2:=0.U
  }.otherwise
  {
    fpu_req_2:=false.B
    fpu_req_fmaCmd_2:=0.U 
    FP_to_posit.io.in:=0.U
    fpu_req_in1_2:=0.U
  }
  io.fpu_resp.ready:=true.B
  fpu_req_in2_2:=0.U
  io.fpu_req.valid:=fpu_req_1 | fpu_req_2
  io.fpu_req.bits.fmaCmd:= fpu_req_fmaCmd_1 | fpu_req_fmaCmd_2
  io.fpu_req.bits.in1:= fpu_req_in1_1 | fpu_req_in1_2
  io.fpu_req.bits.in2:= fpu_req_in2_1 | fpu_req_in2_2
  Int_to_Posit.io.in:=cmd.bits.rs1
  val int_to_posit_res = Int_to_Posit.io.out
  val int_to_posit_dest = cmd.bits.rs2
  val posit_to_int_res = RegInit(0.U(32.W))
  val posit_to_int_rd = RegInit(0.U(32.W))
  val delayed_posit_to_int = RegInit(false.B)
  when(convert_posit_to_int){
    posit_to_int.io.in:=regfile(cmd.bits.rs1) 
    posit_to_int_rd:=cmd.bits.inst.rd 
    posit_to_int_res:=posit_to_int.io.out
  }.otherwise
  {
    posit_to_int.io.in:=0.U 
    posit_to_int_rd:=0.U 
    posit_to_int_res:=0.U
  }
  when(convert_posit_to_int_reg | delayed_posit_to_int)
  {
    when(io.resp.ready)
    {
      io.resp.valid := true.B
      io.resp.bits.rd := posit_to_int_rd
      io.resp.bits.data := posit_to_int_res
      delayed_posit_to_int:=false.B
    }.otherwise
    {
      delayed_posit_to_int:=true.B
      io.resp.valid := false.B
      io.resp.bits.rd:=0.U
      io.resp.bits.data:=0.U
    }
  }.otherwise
  {
    delayed_posit_to_int:=false.B
    io.resp.valid := false.B
    io.resp.bits.rd:=0.U
    io.resp.bits.data:=0.U
  }
  val posit_write_value=RegNext(cmd.bits.rs1)
  val source_1=cmd.bits.rs1(8-1,0)
  val source_2=cmd.bits.rs1(2*8-1,8)
  posit_add_module.io.in1:=regfile(source_1)
  val in2_complement = Wire(UInt(32.W))
  in2_complement:=((~regfile(source_2))+1.U(32.W)) (31,0)
  posit_add_module.io.in2:=Mux(posit_add,regfile(source_2),in2_complement)
  posit_add_module.io.clock:=clock
  posit_add_module.io.start:=posit_add | posit_sub
  posit_add_chisel_module.io.num1:=regfile(source_1)
  posit_add_chisel_module.io.num2:=regfile(source_2)
  posit_add_chisel_module.io.sub:=posit_sub
  posit_mult_module.io.clock:=clock
  posit_mult_module.io.start:=posit_mul
  posit_mult_module.io.in1:=regfile(source_1)
  posit_mult_module.io.in2:=regfile(source_2)
  val posit_mult_res = RegNext(posit_mult_module.io.out)
  bct_posit_div_module.io.in1:=regfile(source_1)
  bct_posit_div_module.io.in2:=regfile(source_2)
  bct_posit_div_module.io.clock:=clock
  bct_posit_div_module.io.reset:=reset
  bct_posit_div_module.io.start:=posit_div | posit_sqrt 
  bct_posit_div_module.io.op := posit_div 
  val posit_div_dest = RegInit(0.U(8.W))
  val block_cmd_2 = RegInit(false.B)
  val block_cmd_3 = RegInit(false.B)
  val block_cmd_4 = RegInit(false.B)
  when(posit_add | posit_sub)
  {
    block_cmd_3:=true.B 
  }.elsewhen(posit_add_module.io.done)
  {
    block_cmd_3:=false.B
  }
  when(posit_mul)
  {
    block_cmd_4:=true.B 
  }.elsewhen(posit_mult_module.io.done)
  {
    block_cmd_4:=false.B
  }

  when(posit_div | posit_sqrt)
  {
    posit_div_dest:=cmd.bits.rs2(7,0)
    block_cmd_2:=true.B 
  }.elsewhen(bct_posit_div_module.io.done)
  {
    block_cmd_2:=false.B
  }
  val posit_mul_dest = RegInit(0.U(8.W))

  val addend = cmd.bits.rs1
  val destination_reg_wire = cmd.bits.rs2
  val destination_reg_wire_reg = RegNext(destination_reg_wire)
  val posit_add_dest = RegInit(0.U(8.W))
  when(posit_add | posit_sub)
  {
    posit_add_dest:=destination_reg_wire
  }
  when(posit_mul)
  {
    posit_mul_dest:=destination_reg_wire
  }

  when( block_cmd_1 | block_cmd_2 | block_cmd_3 | block_cmd_4 | block_cmd_5)
  {
    io.busy := true.B
    cmd.ready := false.B 
  }.otherwise
  {
    io.busy := false.B
    cmd.ready := true.B 
  }
  when (posit_write) { 
    regfile(addr) := cmd.bits.rs1
  }.elsewhen(posit_mult_module.io.done){
    regfile(posit_mul_dest) := posit_mult_module.io.out
  }.elsewhen(bct_posit_div_module.io.done)
  {
    regfile(posit_div_dest) := bct_posit_div_module.io.out
  }.elsewhen(posit_add_module.io.done)
  {   
    regfile(posit_add_dest) := posit_add_module.io.out    
  }.elsewhen(convert_int_to_posit)
  {
    regfile(int_to_posit_dest) := int_to_posit_res
  }.elsewhen(get_fpu_reg & io_fpu_resp_fire)
  {
    regfile(float_to_posit_dest) :=FP_to_posit.io.out
  }
  io.interrupt := false.B
  io.mem.req.valid:=false.B
}


class  TranslatorExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new TranslatorExampleModuleImp(this)
}

class TranslatorExampleModuleImp(outer: TranslatorExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  val req_addr = Reg(UInt(coreMaxAddrBits.W))
  val req_rd = Reg(chiselTypeOf(io.resp.bits.rd))
  val req_offset = req_addr(pgIdxBits - 1, 0)
  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
  val pte = Reg(new PTE)

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)

  io.cmd.ready := (state === s_idle)

  when (io.cmd.fire()) {
    req_rd := io.cmd.bits.inst.rd
    req_addr := io.cmd.bits.rs1
    state := s_ptw_req
  }

  private val ptw = io.ptw(0)

  when (ptw.req.fire()) { state := s_ptw_resp }

  when (state === s_ptw_resp && ptw.resp.valid) {
    pte := ptw.resp.bits.pte
    state := s_resp
  }

  when (io.resp.fire()) { state := s_idle }

  ptw.req.valid := (state === s_ptw_req)
  ptw.req.bits.valid := true.B
  ptw.req.bits.bits.addr := req_vpn

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), -1.S(xLen.W).asUInt)

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
}

class  CharacterCountExample(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new CharacterCountExampleModuleImp(this)
  override val atlNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1("CharacterCountRoCC")))))
}

class CharacterCountExampleModuleImp(outer: CharacterCountExample)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with HasCoreParameters
  with HasL1CacheParameters {
  val cacheParams = tileParams.icache.get

  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)

  val needle = Reg(UInt(8.W))
  val addr = Reg(UInt(coreMaxAddrBits.W))
  val count = Reg(UInt(xLen.W))
  val resp_rd = Reg(chiselTypeOf(io.resp.bits.rd))

  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val offset = addr(blockOffset - 1, 0)
  val next_addr = (addr_block + 1.U) << blockOffset.U

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val (tl_out, edgesOut) = outer.atlNode.out(0)
  val gnt = tl_out.d.bits
  val recv_data = Reg(UInt(cacheDataBits.W))
  val recv_beat = RegInit(0.U(log2Up(cacheDataBeats+1).W))

  val data_bytes = VecInit(Seq.tabulate(cacheDataBits/8) { i => recv_data(8 * (i + 1) - 1, 8 * i) })
  val zero_match = data_bytes.map(_ === 0.U)
  val needle_match = data_bytes.map(_ === needle)
  val first_zero = PriorityEncoder(zero_match)

  val chars_found = PopCount(needle_match.zipWithIndex.map {
    case (matches, i) =>
      val idx = Cat(recv_beat - 1.U, i.U(beatOffset.W))
      matches && idx >= offset && i.U <= first_zero
  })
  val zero_found = zero_match.reduce(_ || _)
  val finished = Reg(Bool())

  io.cmd.ready := (state === s_idle)
  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := resp_rd
  io.resp.bits.data := count
  tl_out.a.valid := (state === s_acq)
  tl_out.a.bits := edgesOut.Get(
                       fromSource = 0.U,
                       toAddress = addr_block << blockOffset,
                       lgSize = lgCacheBlockBytes.U)._2
  tl_out.d.ready := (state === s_gnt)

  when (io.cmd.fire()) {
    addr := io.cmd.bits.rs1
    needle := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
    count := 0.U
    finished := false.B
    state := s_acq
  }

  when (tl_out.a.fire()) { state := s_gnt }

  when (tl_out.d.fire()) {
    recv_beat := recv_beat + 1.U
    recv_data := gnt.data
    state := s_check
  }

  when (state === s_check) {
    when (!finished) {
      count := count + chars_found
    }
    when (zero_found) { finished := true.B }
    when (recv_beat === cacheDataBeats.U) {
      addr := next_addr
      state := Mux(zero_found || finished, s_resp, s_acq)
    } .otherwise {
      state := s_gnt
    }
  }

  when (io.resp.fire()) { state := s_idle }

  io.busy := (state =/= s_idle)
  io.interrupt := false.B
  io.mem.req.valid := false.B
  // Tie off unused channels
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B
}
/*
class BlackBoxExample(opcodes: OpcodeSet, blackBoxFile: String)(implicit p: Parameters)
    extends LazyRoCC(opcodes) {
  override lazy val module = new BlackBoxExampleModuleImp(this, blackBoxFile)
}

class BlackBoxExampleModuleImp(outer: BlackBoxExample, blackBoxFile: String)(implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with RequireSyncReset
    with HasCoreParameters {

  val blackbox = {
    val roccIo = io
    Module(
      new BlackBox( Map( "xLen" -> IntParam(xLen),
                         "PRV_SZ" -> IntParam(PRV.SZ),
                         "coreMaxAddrBits" -> IntParam(coreMaxAddrBits),
                         "dcacheReqTagBits" -> IntParam(roccIo.mem.req.bits.tag.getWidth),
                         "M_SZ" -> IntParam(M_SZ),
                         "mem_req_bits_size_width" -> IntParam(roccIo.mem.req.bits.size.getWidth),
                         "coreDataBits" -> IntParam(coreDataBits),
                         "coreDataBytes" -> IntParam(coreDataBytes),
                         "paddrBits" -> IntParam(paddrBits),
                         "FPConstants_RM_SZ" -> IntParam(FPConstants.RM_SZ),
                         "fLen" -> IntParam(fLen),
                         "FPConstants_FLAGS_SZ" -> IntParam(FPConstants.FLAGS_SZ)
                   ) ) with HasBlackBoxResource {
        val io = IO( new Bundle {
                      val clock = Input(Clock())
                      val reset = Input(Reset())
                      val rocc = chiselTypeOf(roccIo)
                    })
        override def desiredName: String = blackBoxFile
        addResource(s"/vsrc/$blackBoxFile.v")
      }
    )
  }

  blackbox.io.clock := clock
  blackbox.io.reset := reset
  blackbox.io.rocc.cmd <> io.cmd
  io.resp <> blackbox.io.rocc.resp
  io.mem <> blackbox.io.rocc.mem
  io.busy := blackbox.io.rocc.busy
  io.interrupt := blackbox.io.rocc.interrupt
  blackbox.io.rocc.exception := io.exception
  io.ptw <> blackbox.io.rocc.ptw
  io.fpu_req <> blackbox.io.rocc.fpu_req
  blackbox.io.rocc.fpu_resp <> io.fpu_resp

}*/

class OpcodeSet(val opcodes: Seq[UInt]) {
  def |(set: OpcodeSet) =
    new OpcodeSet(this.opcodes ++ set.opcodes)

  def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
}

object OpcodeSet {
  def custom0 = new OpcodeSet(Seq("b0001011".U))
  def custom1 = new OpcodeSet(Seq("b0101011".U))
  def custom2 = new OpcodeSet(Seq("b1011011".U))
  def custom3 = new OpcodeSet(Seq("b1111011".U))
  def all = custom0 | custom1 | custom2 | custom3
}

class RoccCommandRouter(opcodes: Seq[OpcodeSet])(implicit p: Parameters)
    extends CoreModule()(p) {
  val io = new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Vec(opcodes.size, Decoupled(new RoCCCommand))
    val busy = Output(Bool())
  }

  val cmd = Queue(io.in)
  val cmdReadys = io.out.zip(opcodes).map { case (out, opcode) =>
    val me = opcode.matches(cmd.bits.inst.opcode)
    out.valid := cmd.valid && me
    out.bits := cmd.bits
    out.ready && me
  }
  cmd.ready := cmdReadys.reduce(_ || _)
  io.busy := cmd.valid

  assert(PopCount(cmdReadys) <= 1.U,
    "Custom opcode matched for more than one accelerator")
}
