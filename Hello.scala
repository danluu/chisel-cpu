import Chisel._

class HelloModule extends Module { 
  val io = new Bundle { 
    val in = Bool(INPUT)
    val out = Bool(OUTPUT)
  }
  printf("Hello world!!!!!\n")

  // Workaround for chisel bug that causes printf output to be destroyed
  val r = Reg(next = io.in)
  io.out := r
  printf("Workaround print%b\n", io.in)
}

class HelloModuleTests(c: HelloModule) extends Tester(c, Array(c.io)) { 
  defTests {
    true
  }
}

object hello { 
  def main(args: Array[String]): Unit = { 
    chiselMainTest(Array[String]("--backend", "c", "--genHarness"),
		   () => Module(new HelloModule())){ c => new HelloModuleTests(c)}
  }
}
