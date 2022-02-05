package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import posit.function._
import posit.function.Shift._
import posit.convert._


/*
                    table of mantissa quotient
          
poly          positive                     negative
range         (0.5, 2)                    [-2, -0.5]        
        decimal      binary          decimal       binary     
          0.5     doesn't exist       -0.5      111.10000...
          0.f      000.1xxxx...       -0.f      111.0xxxx...
          1.0      001.00000...       -1.0      111.00000...
          1.f      001.xxxxx...       -1.f      110.xxxxx...
          2.0     doesn't exist       -2.0      110.00000...
*/


// 2's complement
class PositDivisionSqrt(nBits: Int, eBits: Int) extends Module {

  override def desiredName = s"PositDivSqrter${nBits}_${eBits}"

////////////////////////////////////////////////////////////////////////////
///////////////////////          parameters          ///////////////////////
////////////////////////////////////////////////////////////////////////////

  // posit
  val regimeWidth = log2Ceil(nBits - 1) + 1  // 1bit sign
  val fracWidth   = nBits - eBits - 3
  val sigWidth    = fracWidth + 1
  val maxScale    = (1 << eBits) * (nBits - 2)
  val minScale    = -maxScale - 1

////////////////////////////////////////////////////////////////////////////
///////////////////////              io              ///////////////////////
////////////////////////////////////////////////////////////////////////////

  val io = IO(new Bundle {
    /*--------------------------------------------------------------------
    *--------------------------------------------------------------------*/
    val inReady    = Output(Bool())
    val inValid    = Input(Bool())
    val sqrtOp     = Input(Bool())
    val A          = Input(UInt(nBits.W))
    val B          = Input(UInt(nBits.W))
    /*--------------------------------------------------------------------
    *--------------------------------------------------------------------*/
    val diviValid  = Output(Bool())
    val sqrtValid  = Output(Bool())
    val invalidExc = Output(Bool())
    val Q          = Output(UInt(nBits.W))
  })

  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/

  val cycleNum  = RegInit(0.U(log2Up(sigWidth + 6).W))

  val sqrtOp_Z  = Reg(Bool())
  val isNaR_Z   = Reg(Bool())
  val isZero_Z  = Reg(Bool())
  val scale_Z   = Reg(SInt((regimeWidth + eBits + 1).W))
	val signB_Z   = Reg(Bool())
  val fractB_Z  = Reg(UInt(fracWidth.W))
  val rem_Z     = Reg(UInt((sigWidth + 6).W))
  val sigX_Z    = Reg(UInt((sigWidth + 6).W))

  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/

  val decA = RawFloatFromPosit(io.A, nBits, eBits)
  val decB = RawFloatFromPosit(io.B, nBits, eBits)

  val sigA_S = Cat(Fill(3, decA.sign), ~decA.sign, decA.fraction, 0.U(3.W))
  // sigB_S = sigB << 2
  val sigB_S = Cat(decB.sign, ~decB.sign, decB.fraction, 0.U(5.W))

  val invalidDivision = decB.isZero
  val invalidSqrt     = !decA.isNaR && decA.sign    
  val isNaR_S         = Mux(
                              io.sqrtOp,
                              decA.isNaR || invalidSqrt,    
                              decA.isNaR || decB.isNaR || invalidDivision
                            )
  val isZero_S        = Mux(
                              io.sqrtOp, 
                              decA.isZero, 
                              decA.isZero && !decB.isZero && !decB.isNaR
                            )

  val specialCaseA_S    = decA.isNaR || decA.isZero
  val specialCaseB_S    = decB.isNaR || decB.isZero
  val normalCase_S_div  = !specialCaseA_S && !specialCaseB_S
  val normalCase_S_sqrt = !specialCaseA_S && !decA.sign
  val normalCase_S      = Mux(io.sqrtOp, normalCase_S_sqrt, normalCase_S_div)

  val sExpQuot_S_div    = decA.scale -& decB.scale
  val oddSqrt_S         = io.sqrtOp && decA.scale(0)

  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/

  val idle                = (cycleNum === 0.U)
  val ready               = (cycleNum <= 1.U)
  val entering            = ready && io.inValid
  val entering_normalCase = entering && normalCase_S
  val scaleNotChange      = sigX_Z(sigWidth + 5) ^ sigX_Z(sigWidth + 3)
  val skipCycle2          = (cycleNum === 3.U) && scaleNotChange

  when(!idle || io.inValid) {
    cycleNum := Mux(entering & !normalCase_S, 1.U,        0.U) |
                Mux(entering_normalCase,      
                      Mux(io.sqrtOp, 
                            (sigWidth + 4).U, 
                            (sigWidth + 6).U
                          ),                              0.U) |
                Mux(!idle && !skipCycle2, cycleNum - 1.U, 0.U) |
                Mux(!idle &&  skipCycle2, 1.U,            0.U)
  }

  when(entering) {
    sqrtOp_Z := io.sqrtOp
    isNaR_Z  := isNaR_S
    isZero_Z := isZero_S
  }

  when(entering_normalCase) {
    scale_Z := Mux(io.sqrtOp, decA.scale >> 1, sExpQuot_S_div)
  }

  when(entering_normalCase && !io.sqrtOp) {
		signB_Z  := decB.sign
    fractB_Z := decB.fraction
  }

  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/

  val bitMask = Wire(UInt((sigWidth + 5).W))
  bitMask    := (1.U << cycleNum) >> 2

  val rem = Mux(ready && !oddSqrt_S, sigA_S,                0.U) |
            Mux(ready &&  oddSqrt_S, (sigA_S << 1).tail(1), 0.U) |
            Mux(!ready,              rem_Z,                 0.U)

  val trialTerm = Mux(ready && !io.sqrtOp, sigB_S,                                     0.U) |
                  Mux(ready &&  io.sqrtOp, (BigInt(1) << (fracWidth + 3)).U,           0.U) |
                  Mux(!ready && !sqrtOp_Z, Cat(signB_Z, ~signB_Z, fractB_Z, 0.U(5.W)), 0.U) | 
                  Mux(!ready &&  sqrtOp_Z, sigX_Z | 
                                           (bitMask & Fill(fracWidth + 3, rem.head(1))) | 
                                           (bitMask >> 1),                             0.U)

	val trialRem = Mux(
                      ready,
											Mux(
                          (rem.head(1) ^ trialTerm.head(1)).asBool, 
											    rem + trialTerm,
											    rem - trialTerm
										    ),
                      Mux(
                          (rem.head(1) ^ trialTerm.head(1)).asBool, 
											    (rem << 1).tail(1) + trialTerm,
											    (rem << 1).tail(1) - trialTerm
										    )
                    )

	val trIsZero  = !trialRem.orR
	val remIsZero = !rem.orR
  val newBit    = MuxCase(
                            ~(trialTerm.head(1) ^ trialRem.head(1)),
										        Array(
                                    trIsZero -> ~sigX_Z.head(1),
							 			                remIsZero -> sigX_Z.head(1)
                                  )
									        )
							
  when(entering_normalCase || (cycleNum > 2.U) && !remIsZero) {
    rem_Z := trialRem
  }

  when(entering_normalCase || (!ready && newBit.asBool)) {
    sigX_Z := Mux(ready && !io.sqrtOp, newBit << (sigWidth + 5),        0.U) |
              Mux(ready &&  io.sqrtOp, (BigInt(1) << (sigWidth + 3)).U, 0.U) |
              Mux(!ready,              sigX_Z | bitMask,                0.U)
  }

  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/

  val sigXBias = Mux(scaleNotChange, sigX_Z.head(1) << 1, sigX_Z.head(1))
  val realSigX = sigX_Z + sigXBias
  val realFrac = Mux(
											scaleNotChange, 
											realSigX(fracWidth + 3, 4), 
											realSigX(fracWidth + 2, 3)
										) 

  val decQ          = Wire(new RawFloat(nBits, eBits))
  val scaleNeedSub  = ~(realSigX(sigWidth + 5) ^ realSigX(sigWidth + 3))
  val notNeedSubTwo = realSigX(sigWidth + 5) ^ realSigX(sigWidth + 2)
  val scaleSubOne   = scaleNeedSub & notNeedSubTwo
  val scaleSubTwo   = scaleNeedSub & ~notNeedSubTwo
  val realExp       = scale_Z - Cat(scaleSubTwo, scaleSubOne).zext 
  val underflow     = realExp < minScale.S
  val overflow      = realExp > maxScale.S

  decQ.isNaR    := isNaR_Z
  decQ.isZero   := isZero_Z
  decQ.sign     := realSigX.head(1)
	decQ.scale    := MuxCase(
                            realExp, 
                            Array(
                                    overflow  -> maxScale.S,
                                    underflow -> minScale.S
                                  )
                   			  ) 
  decQ.fraction := realFrac
  decQ.grs      := Mux(scaleNotChange, realSigX(3, 1), realSigX(2, 0))

/*----------------------------------------------------------------------------
*----------------------------------------------------------------------------*/

  val outValid = (cycleNum === 1.U)

  io.inReady    := ready
  io.diviValid  := outValid && !sqrtOp_Z
  io.sqrtValid  := outValid &&  sqrtOp_Z
  io.invalidExc := isNaR_Z
  io.Q          := RawFloatToPosit(decQ)  
}


class PositAddCore(val nbits: Int, val es: Int) extends Module with HasHardPositParams {

  val io = IO(new Bundle{
    val num1   = Input(new unpackedPosit(nbits, es))
    val num2   = Input(new unpackedPosit(nbits, es))
    val sub    = Input(Bool())

    val trailingBits = Output(UInt(trailingBitCount.W))
    val stickyBit = Output(Bool())
    val out    = Output(new unpackedPosit(nbits, es))
  })

  val num1 = io.num1
  val num2 = io.num2

  val result = Wire(new unpackedPosit(nbits, es))

  val num1magGt =
    (num1.exponent > num2.exponent) |
     (num1.exponent === num2.exponent &&
      (num1.fraction > num2.fraction))
  val num2AdjSign = num2.sign ^ io.sub

  val largeSign = Mux(num1magGt, num1.sign, num2AdjSign)
  val largeExp  = Mux(num1magGt, num1.exponent, num2.exponent)
  val largeFrac =
    Cat(Mux(num1magGt, num1.fraction, num2.fraction), 0.U((maxAdderFractionBits - maxFractionBitsWithHiddenBit - 1).W))

  val smallSign = Mux(num1magGt, num2AdjSign, num1.sign)
  val smallExp  = Mux(num1magGt, num2.exponent, num1.exponent)
  val smallFrac =
    Cat(Mux(num1magGt, num2.fraction, num1.fraction), 0.U((maxAdderFractionBits - maxFractionBitsWithHiddenBit - 1).W))

  val expDiff = (largeExp - smallExp).asUInt()
  val shiftedSmallFrac =
    Mux(expDiff < (maxAdderFractionBits - 1).U, smallFrac >> expDiff, 0.U)
  val smallFracStickyBit =
    Mux(expDiff > (maxAdderFractionBits - maxFractionBitsWithHiddenBit - 1).U,
    (smallFrac & ((1.U << (expDiff - (maxAdderFractionBits - maxFractionBitsWithHiddenBit - 1).U)) - 1.U)).orR(), false.B)

  val isAddition = !(largeSign ^ smallSign)
  val signedSmallerFrac =
    Mux(isAddition, shiftedSmallFrac, ~shiftedSmallFrac + 1.U)
  val adderFrac =
    WireInit(UInt(maxAdderFractionBits.W), largeFrac +& signedSmallerFrac)

  val sumOverflow = isAddition & adderFrac(maxAdderFractionBits - 1)

  val adjAdderExp = largeExp - sumOverflow.asSInt()
  val adjAdderFrac =
    Mux(sumOverflow, adderFrac(maxAdderFractionBits - 1, 1), adderFrac(maxAdderFractionBits - 2, 0))
  val sumStickyBit = sumOverflow & adderFrac(0)

  val normalizationFactor = countLeadingZeros(adjAdderFrac)

  val normExponent = adjAdderExp - normalizationFactor.asSInt()
  val normFraction = adjAdderFrac << normalizationFactor.asUInt()

  result.isNaR    := num1.isNaR || num2.isNaR
  result.isZero   := (num1.isZero && num2.isZero) | (adderFrac === 0.U)
  result.sign     := largeSign
  result.exponent := normExponent
  result.fraction := normFraction(maxAdderFractionBits - 2, maxAdderFractionBits - maxFractionBitsWithHiddenBit - 1)

  io.trailingBits := normFraction(maxAdderFractionBits - maxFractionBitsWithHiddenBit - 2, maxAdderFractionBits - maxFractionBitsWithHiddenBit - trailingBitCount - 1)
  io.stickyBit    := sumStickyBit | normFraction(stickyBitCount - 1, 0).orR()

  io.out := result
}

class PositAdd(val nbits: Int, val es: Int) extends Module with HasHardPositParams {
  require(nbits > es)
  require(es >= 0)

  val io = IO(new Bundle{
    val num1   = Input(UInt(nbits.W))
    val num2   = Input(UInt(nbits.W))
    val sub    = Input(Bool())

    val isZero = Output(Bool())
    val isNaR  = Output(Bool())
    val out    = Output(UInt(nbits.W))
  })

  val positAddCore = Module(new PositAddCore(nbits, es))

  val num1Extractor = Module(new PositExtractor(nbits, es))
  val num2Extractor = Module(new PositExtractor(nbits, es))
  num1Extractor.io.in := io.num1
  num2Extractor.io.in := io.num2

  positAddCore.io.num1 := num1Extractor.io.out
  positAddCore.io.num2 := num2Extractor.io.out
  positAddCore.io.sub  := io.sub

  private val positGenerator = Module(new PositGenerator(nbits, es))
  positGenerator.io.in           := positAddCore.io.out
  positGenerator.io.trailingBits := positAddCore.io.trailingBits
  positGenerator.io.stickyBit    := positAddCore.io.stickyBit

  io.isZero := positAddCore.io.out.isZero | isZero(positGenerator.io.out)
  io.isNaR  := positAddCore.io.out.isNaR  | isNaR(positGenerator.io.out)
  io.out    := positGenerator.io.out
}

class PositExtractor(val nbits: Int, val es: Int) extends Module with HasHardPositParams {
  val io = IO(new Bundle {
    val in  = Input(UInt(nbits.W))
    val out = Output(new unpackedPosit(nbits, es))
  })

  val sign   = io.in(nbits - 1)
  val absIn  = Mux(sign, ~io.in + 1.U, io.in).asUInt()
  val negExp = ~absIn(nbits - 2)

  val regExpFrac  = absIn(nbits - 2, 0)
  val zerosRegime = Mux(negExp, regExpFrac, ~regExpFrac)
  val regimeCount =
    Cat(0.U(1.W),
      Mux(isZero(zerosRegime), (nbits - 1).U, countLeadingZeros(zerosRegime)))
  val regime =
    Mux(negExp, ~regimeCount + 1.U, regimeCount - 1.U)

  val expFrac = absIn << (regimeCount + 2.U)
  val extractedExp =
    if (es > 0) expFrac(nbits - 1, nbits - es)
    else 0.U
  val frac = expFrac(nbits - es - 1, nbits - es - maxFractionBits)

  io.out.isNaR    := isNaR(io.in)
  io.out.isZero   := isZero(io.in)

  io.out.sign     := sign
  io.out.exponent := {
    if (es > 0) Cat(regime, extractedExp)
    else regime
  }.asSInt()
  io.out.fraction := Cat(1.U, frac)
}

class unpackedPosit(val nbits: Int, val es: Int) extends Bundle with HasHardPositParams {

  val sign = Bool()
  val exponent = SInt(maxExponentBits.W)
  val fraction = UInt(maxFractionBitsWithHiddenBit.W) //TODO Transfer only fraction bits without hidden bit
  val isZero = Bool()
  val isNaR = Bool()

  override def cloneType =
    new unpackedPosit(nbits, es).asInstanceOf[this.type]
}

object countLeadingZeros {
  def apply(in: UInt): UInt = PriorityEncoder(in.asBools.reverse)
}

object lowerBitMask {
  def apply(in: UInt): UInt = UIntToOH(in) - 1.U
}

object isOnlyMSBSet {
  def apply(num: UInt, n: Int): Bool = num(n - 1) & ~num(n - 2, 0).orR()
}

trait HasHardPositParams {
  val nbits: Int
  val es: Int

  require(nbits > es + 3, s"Posit size $nbits is inadequate for exponent size $es")
  require(trailingBitCount >= 2, "At-least 2 trailing bits required")

  def maxExponentBits: Int = getMaxExponentBits(nbits, es)

  def maxFractionBits: Int = getMaxFractionBits(nbits, es)

  def maxFractionBitsWithHiddenBit: Int = getMaxFractionBitsWithHiddenBit(nbits, es)

  def getMaxExponentBits(n: Int, e: Int): Int = log2Ceil(n) + e + 2

  def getMaxFractionBits(n: Int, e: Int): Int = if (e + 3 >= n) 1 else n - 3 - e

  def getMaxFractionBitsWithHiddenBit(n: Int, e: Int): Int = getMaxFractionBits(n, e) + 1

  def maxAdderFractionBits: Int = maxFractionBitsWithHiddenBit + trailingBitCount + stickyBitCount + 1

  def maxMultiplierFractionBits: Int = 2 * maxFractionBitsWithHiddenBit

  def maxDividerFractionBits: Int = maxFractionBitsWithHiddenBit + trailingBitCount + stickyBitCount + 1

  def NaR: UInt = (1.U << (nbits - 1)).asUInt()

  def zero: UInt = 0.U(nbits.W)

  def isNaR(num: UInt): Bool = isOnlyMSBSet(num, nbits)

  def isZero(num: UInt): Bool = ~num.orR()

  def trailingBitCount = 2

  def stickyBitCount = 1

  def maxRegime: Int = nbits - 2

  def minRegime: Int = -maxRegime

  def maxExponent: Int = maxRegime * (1 << es)

  def minExponent: Int = minRegime * (1 << es)

  def maxSignedInteger(w: Int): Int = (1 << (w - 1)) - 1

  def maxUnsignedInteger(w: Int): Int = (1 << w) - 1
}

class PositGenerator(val nbits: Int, val es: Int) extends Module with HasHardPositParams {

  val io = IO(new Bundle {
    val in = Input(new unpackedPosit(nbits, es))
    val trailingBits = Input(UInt(trailingBitCount.W))
    val stickyBit = Input(Bool())
    val out = Output(UInt(nbits.W))
  })

  val fraction = io.in.fraction(maxFractionBits - 1, 0)
  val negExp = io.in.exponent < 0.S

  val regime =
    Mux(negExp, -io.in.exponent(maxExponentBits - 1, es), io.in.exponent(maxExponentBits - 1, es)).asUInt()
  val exponent = io.in.exponent(if (es > 0) es - 1 else 0, 0)
  val offset =
    regime - (negExp & regime =/= (nbits - 1).U)

  val expFrac =
    if (es > 0)
      Cat(Mux(negExp, 1.U(2.W), 2.U(2.W)), exponent, fraction, io.trailingBits).asSInt()
    else
      Cat(Mux(negExp, 1.U(2.W), 2.U(2.W)), fraction, io.trailingBits).asSInt()

  //u => un ; T => Trimmed ; R => Rounded ; S => Signed
  val uT_uS_posit = (expFrac >> offset)(nbits - 2 + trailingBitCount, 0).asUInt()
  val uR_uS_posit = uT_uS_posit(nbits - 2 + trailingBitCount, trailingBitCount)

  val stickyBitMask = lowerBitMask(offset)(nbits - 3, 0)
  val gr =
    uT_uS_posit(trailingBitCount - 1, trailingBitCount - 2)
  val stickyBit =
    io.stickyBit | (expFrac.asUInt() & stickyBitMask).orR() | {
      if (trailingBitCount > 2) uT_uS_posit(trailingBitCount - 3, 0).orR()
      else false.B
    }
  val roundingBit =
    Mux(uR_uS_posit.andR(), false.B,
      gr(1) & ~(~uR_uS_posit(0) & gr(1) & ~gr(0) & ~stickyBit))
  val R_uS_posit = uR_uS_posit + roundingBit

  //Underflow Correction
  val uFC_R_uS_posit =
    Cat(0.U(1.W), R_uS_posit | isZero(R_uS_posit))

  val R_S_posit =
    Mux(io.in.sign, ~uFC_R_uS_posit + 1.U, uFC_R_uS_posit)

  io.out := Mux(io.in.isNaR, NaR,
    Mux((io.in.fraction === 0.U) | io.in.isZero, zero, R_S_posit))
}
