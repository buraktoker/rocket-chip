package freechips.rocketchip.tile

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import chisel3.experimental.IntParam
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter

class posit_to_int( val N : Int , val out_N : Int) extends BlackBox(Map("N" -> N,
                                  "out_N" -> out_N)) {
  val io = IO(new Bundle {
    val in = Input(UInt(N.W))
    val out = Output(UInt(out_N.W))
    val overflow = Output(Clock())
  })
}


class Int_to_Posit( val N : Int , val es : Int) extends BlackBox(Map("N" -> N,
                                  "es" -> es)) {
  val io = IO(new Bundle {
    val in = Input(UInt(N.W))
    val out = Output(UInt(N.W))
  })
}

class posit_to_float( val N : Int , val es : Int) extends BlackBox(Map("N" -> N,"es" -> es))
{
  val io = IO(new Bundle {
    val in = Input(UInt(N.W))
    val out = Output(UInt(N.W))
  })
}

class FP_to_posit( val N : Int , val E: Int,val es : Int) extends BlackBox(Map("N" -> N,"E"->E,"es" -> es))
{
  val io = IO(new Bundle {
    val in = Input(UInt(N.W))
    val out = Output(UInt(N.W))
  })
}


class posit_add( val N : Int , val es : Int) extends BlackBox(Map("N" -> N,
                                  "es" -> es)) {
  val io = IO(new Bundle {
    val in1 = Input(UInt(N.W))
    val in2 = Input(UInt(N.W))
    val clock = Input(Clock())
    val start = Input(Bool())
    val out = Output(UInt(N.W))
    val inf = Output(Bool())
    val zero = Output(Bool())
    val done = Output(Bool())
  })
}


class posit_mult( val N : Int , val es : Int) extends BlackBox(Map("N" -> N,
                                  "es" -> es)) {
  val io = IO(new Bundle {
    val in1 = Input(UInt(N.W))
    val in2 = Input(UInt(N.W))
    val clock = Input(Clock())
    val start = Input(Bool())
    val out = Output(UInt(N.W))
    val inf = Output(Bool())
    val zero = Output(Bool())
    val done = Output(Bool())
  })
}

class bct_posit_div( val N : Int , val es : Int) extends BlackBox(Map("N" -> N,
                                  "es" -> es)) {
  val io = IO(new Bundle {
    val in1 = Input(UInt(N.W))
    val in2 = Input(UInt(N.W))
    val clock = Input(Clock())
    val start = Input(Bool())
    val reset = Input(Reset())
    val out = Output(UInt(N.W))
    val inf = Output(Bool())
    val zero = Output(Bool())
    val done = Output(Bool())
  })
}

