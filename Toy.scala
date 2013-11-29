package Toy

import Chisel._
import Node._
import scala.collection.mutable.HashMap

class Toy extends Module { 
  val io = new Bundle { 
    val jtagWr = Bool(INPUT)
    val jtagAddr = UInt(INPUT, 8)
    val jtagData = Bits(INPUT, 32)
    val out = Bits(OUTPUT, 32)
    val in = Bits(INPUT, 1)
  }
  val pc = Reg(init=UInt(0,8))
  val imem = Mem(Bits(width = 32), 256)
  val rf   = Mem(Bits(width = 32), 128)
  val inst = imem(pc)

  // World's dumbest instruction encoding
  val rw = inst(31, 27)
  val ldImm = inst(26) // Treat (25,0) as Immediate constant
  val ra = inst(25, 21)
  val rb = inst(20, 16)
  val nand = inst(15)
  val add = inst(14)
  val shl = inst(13)
  val shr = inst(12)
  val ld = inst(11)
  val st = inst(10)
  val jz = inst(9)
  
  io.out := pc
  pc := Mux(ra === Bits(0), pc + UInt(4), pc + rb)

  when (io.jtagWr) { 
    imem(io.jtagAddr) := io.jtagData
  } .otherwise {
    when (ldImm) { 
      rf(rw) := inst(25,0)
    } .elsewhen (nand === Bits(1)) { 
      rf(rw) := ~(rf(ra) & rf(rb))
    } .elsewhen (add === Bits(1)) { 
      rf(rw) := rf(ra) + rf(rb)
    } .elsewhen (shl === Bits(1)) { 
      rf(rw) := rf(ra) << rf(rb)
    } .elsewhen (shr === Bits(1)) { 
      rf(rw) := rf(ra) >> rf(rb)
    } .elsewhen (ld === Bits(1)) { 
      rf(rw) := imem(rf(ra))
    } .elsewhen (st === Bits(1)) { 
      imem(rf(ra)) := rf(rb)
    }
  }
}

class ToyTest(c: Toy) extends Tester(c, Array(c.io)) { 
  defTests { 
    var allGood = true
    val vars = new HashMap[Node, Node]()
    def Op(rw: Int, ra: Int, rb: Int, setBit: Int) = 
      Cat(UInt(rw, 5), UInt(0, 1), UInt(ra, 5), UInt(rb, 5)) | (UInt(1,32) << UInt(setBit))
    def LdImm(rw: Int, imm: Int) = Cat(UInt(rw, 5), UInt(1, 1), UInt(imm, 26))
    val initial = Array(LdImm(0, 8),
			Op(0, 0, 0, 15),
			Op(1, 1, 1, 15),
			Op(0, 0, 0, 9))
    for (x <- 0 until 16) { 
      vars(c.io.in) = Bits(x)
      vars(c.io.out) = UInt((x%4) * 4) 
      allGood &&= step(vars)
    }
    allGood
  }
}

object Toy { 
  def main(args: Array[String]): Unit = { 
    chiselMainTest(Array[String]("--backend", "c", "--genHarness", "--compile", "--test", "--targetDir", "../emulator"),
		   () => Module(new Toy())){ c => new ToyTest(c)}
  }
}
