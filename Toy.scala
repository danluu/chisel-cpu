package Toy

import Chisel._
import Node._
import scala.collection.mutable.HashMap

class Toy extends Module { 
  val io = new Bundle { 
    val out = Bits(OUTPUT, 1)
    val in = Bits(INPUT, 1)
  }
  val pc = Reg(init=UInt(0,8))
  
  io.out := Bits(1)
  pc := pc + UInt(1)
}

class ToyTest(c: Toy) extends Tester(c, Array(c.io)) { 
  defTests { 
    var allGood = true
    val vars = new HashMap[Node, Node]()
    for (x <- 0 until 10) { 
      vars(c.io.in) = Bits(x)
      vars(c.io.out) = Bits(1)
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
