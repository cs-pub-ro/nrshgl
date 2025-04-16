package ro.upb.nrs.hgl

import chisel3._
import chisel3.testers._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester
import java.io._
import fixedpoint._
import ro.upb.nrs.sl
import ro.upb.nrs.hgl.performance
import chiseltest._


class MatrixMultiplyFloatP extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrix multiply"

    it should "multiply 2 matrices" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.FloatP(None, 8, 23), () => new ro.upb.nrs.hgl.performance.FloatP(None, 8, 23), 3)) { c =>
            
            def initMatrix(ncols: Int, nrows: Int) : Array[Int] = {
                Array.tabulate(ncols * nrows) { x => x + 1 }
            }

            for (fileNo <- 0 until 1) {

                val a = scala.io.Source.fromFile(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/tests/test$fileNo.txt").getLines().toArray.filter(_ != "\n")
                val b = a.slice(0, 7)
                
                var matrix1 = Seq("")
                var matrix2 = Seq("")

                for (line <- 0 until (b.size / 2)) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size) {
                        matrix1 ++= Seq(tokens(i))
                    }
                }

                var matrix1filtered = matrix1.slice(1, 10).map(x => "b" + (ro.upb.nrs.sl.FixedFloatingPoint(x.toDouble).toBinaryString))

                for (line <- (b.size / 2 + 1) until b.size) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size){
                        matrix2 ++= Seq(tokens(i))
                    }
                }

                var matrix2filtered = matrix2.slice(1, 10).map(x => "b" + (ro.upb.nrs.sl.FixedFloatingPoint(x.toDouble).toBinaryString))

                for (idx <- 0 until 3 * 3) {
                    c.io.matrix1(idx).value.poke(matrix1filtered(idx).U)
                    c.io.matrix2(idx).value.poke(matrix2filtered(idx).U)
                }

                val writer = new PrintWriter(new File(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/FloatP/test$fileNo.out"))

                c.clock.step(1)

                for (idx <- 0 until 3 * 3) {
                    val x = c.io.matrixRes(idx).value.peek()
                    println(x.litValue)
                    val y = x.asBools.reverse
                    val z = y.map(x => x.litValue).foldLeft("")(_ + _)
                    println(z, z.length)
                    println(ro.upb.nrs.sl.FixedFloatingPoint(z, 8, 23, ro.upb.nrs.sl.RoundEven).toBigDecimal.toString)
                    writer.write(ro.upb.nrs.sl.FixedFloatingPoint(z, 8, 23, ro.upb.nrs.sl.RoundEven).toBigDecimal.toString + " ")
                    if (idx % 3 == 2) {
                        writer.write("\n")
                    }    
                }

                writer.close()
            }
        }
    }
}


class MatrixMultiplyNatural extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrices with natural elements"


    it should "multiply 2 matrices" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.Natural(None, 10), () => new ro.upb.nrs.hgl.performance.Natural(None, 10), 3)) { c =>
            
            def initMatrix(ncols: Int, nrows: Int) : Array[Int] = {
                Array.tabulate(ncols * nrows) { x => x + 1 }
            }

            for (idx <- 0 until 3 * 3) {
                c.io.matrix1(idx).value.poke(12.U)
                c.io.matrix2(idx).value.poke(15.U)
            }

            c.clock.step()

            for (idx <- 0 until 3 * 3) {
                c.io.matrixRes(idx).value.expect(240.U)    
            }
        }
    }
}

class MatrixMultiplyInteger extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrices with integer elements"


    it should "multiply 2 matrices" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.Integer(None, 10), () => new ro.upb.nrs.hgl.performance.Integer(None, 10), 3)) { c =>
            
            def initMatrix(ncols: Int, nrows: Int) : Array[Int] = {
                Array.tabulate(ncols * nrows) { x => x + 1 }
            }

            for (idx <- 0 until 3 * 3) {
                c.io.matrix1(idx).value.poke(12.S)
                c.io.matrix2(idx).value.poke(15.S)
            }

            c.clock.step()

            for (idx <- 0 until 3 * 3) {
                c.io.matrixRes(idx).value.expect(540.S)    
            }
        }
    }
}

class MatrixMultiplyPosit extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrices with integer elements"


    it should "multiply 2 matrices" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.Posit(None, 1 , 8), () => new ro.upb.nrs.hgl.performance.Posit(None, 1, 8), 3)) { c =>
            
            def initMatrix(ncols: Int, nrows: Int) : Array[Int] = {
                Array.tabulate(ncols * nrows) { x => x + 1 }
            }

            for (fileNo <- 0 until 1) {

                val a = scala.io.Source.fromFile(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/tests/test$fileNo.txt").getLines().toArray.filter(_ != "\n")
                val b = a.slice(0, 7)
                
                var matrix1 = Seq("")
                var matrix2 = Seq("")

                for (line <- 0 until (b.size / 2)) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size) {
                        matrix1 ++= Seq(tokens(i))
                    }
                }

                var matrix1filtered = matrix1.slice(1, 10).map(x => "b" + (ro.upb.nrs.sl.Posit(x.toDouble, 1, 8, ro.upb.nrs.sl.RoundEven).toBinaryString))

                for (line <- (b.size / 2 + 1) until b.size) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size){
                        matrix2 ++= Seq(tokens(i))
                    }
                }

                var matrix2filtered = matrix2.slice(1, 10).map(x => "b" + (ro.upb.nrs.sl.Posit(x.toDouble, 1, 8, ro.upb.nrs.sl.RoundEven).toBinaryString))

                for (idx <- 0 until 3 * 3) {
                    c.io.matrix1(idx).value.poke(matrix1filtered(idx).U)
                    c.io.matrix2(idx).value.poke(matrix2filtered(idx).U)
                }
                
                val writer = new PrintWriter(new File(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/Posit8_1/test$fileNo.out"))

                c.clock.step()

                for (idx <- 0 until 3 * 3) {
                    val x = c.io.matrixRes(idx).value.peek()
                    val y = x.asBools.reverse
                    val z = y.map(v => v.litValue).foldLeft("")(_ + _)
                    writer.write(ro.upb.nrs.sl.Posit(z, 1, 8, ro.upb.nrs.sl.RoundEven).toBigDecimal.toString + " ")
                    if (idx % 3 == 2) {
                        writer.write("\n")
                    }    
                }

                writer.close()
            }
        }
    }
}


class MatrixMultiplyFixedPoint extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrices with integer elements"


    it should "multiply 2 matrices" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.FixedPointQ(None, 16, 8), () => new ro.upb.nrs.hgl.performance.FixedPointQ(None, 16, 8), 3)) { c =>

        for (fileNo <- 0 until 50) {

                val a = scala.io.Source.fromFile(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/tests/test$fileNo.txt").getLines().toArray.filter(_ != "\n")
                val b = a.slice(0, 7)
                
                var matrix1 = Seq("")
                var matrix2 = Seq("")

                for (line <- 0 until (b.size / 2)) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size) {
                        matrix1 ++= Seq(tokens(i))
                    }
                }

                var matrix1filtered = matrix1.slice(1, 10)

                for (line <- (b.size / 2 + 1) until b.size) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size){
                        matrix2 ++= Seq(tokens(i))
                    }
                }

                var matrix2filtered = matrix2.slice(1, 10)

                for (idx <- 0 until 3 * 3) {
                    c.io.matrix1(idx).value.poke(FixedPoint.fromBigDecimal(BigDecimal(matrix1filtered(idx)), 16.W, 8.BP))
                    c.io.matrix2(idx).value.poke(FixedPoint.fromBigDecimal(BigDecimal(matrix2filtered(idx)), 16.W, 8.BP))
                }
                
                val writer = new PrintWriter(new File(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/FixedPoint8_8/test$fileNo.out"))

                c.clock.step()

                for (idx <- 0 until 3 * 3) {
                    val x = c.io.matrixRes(idx).value.peek()
                    writer.write(FixedPoint.toBigDecimal(x.litValue, 8).toString + " ")
                    if (idx % 3 == 2) {
                        writer.write("\n")
                    }
                }

                writer.close()
            }
        }
    }
}

class MatrixMultiplyFractional extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrices with integer elements"


    it should "multiply 2 matrices" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.Fractional(None, 14, 17), () => new ro.upb.nrs.hgl.performance.Fractional(None, 14, 17), 3)) { c =>
            
            def initMatrix(ncols: Int, nrows: Int) : Array[Int] = {
                Array.tabulate(ncols * nrows) { x => x + 1 }
            }

            for (fileNo <- 0 until 50) {

                val a = scala.io.Source.fromFile(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/tests/test$fileNo.txt").getLines().toArray.filter(_ != "\n")
                val b = a.slice(0, 7)
                
                var matrix1 = Seq("")
                var matrix2 = Seq("")

                for (line <- 0 until (b.size / 2)) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size) {
                        matrix1 ++= Seq(tokens(i))
                    }
                }

                var matrix1filtered = matrix1.slice(1, 10).map(x => "b" + (ro.upb.nrs.sl.FractionalNumber(x.toDouble, 14, 17, ro.upb.nrs.sl.RoundEven).toBinaryString))

                for (line <- (b.size / 2 + 1) until b.size) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size){
                        matrix2 ++= Seq(tokens(i))
                    }
                }

                var matrix2filtered = matrix2.slice(1, 10).map(x => "b" + (ro.upb.nrs.sl.FractionalNumber(x.toDouble, 14, 17, ro.upb.nrs.sl.RoundEven).toBinaryString))

                for (idx <- 0 until 3 * 3) {
                    c.io.matrix1(idx).value.poke(matrix1filtered(idx).U)
                    c.io.matrix2(idx).value.poke(matrix2filtered(idx).U)
                }
                
                val writer = new PrintWriter(new File(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/Fractional14_17/test$fileNo.out"))

                c.clock.step()

                for (idx <- 0 until 3 * 3) {
                    val x = c.io.matrixRes(idx).value.peek()
                    val y = x.asBools.reverse
                    val z = y.map(v => v.litValue).foldLeft("")(_ + _)
                    //println(z, z.length)
                    writer.write(ro.upb.nrs.sl.FractionalNumber(z, 14, 17, ro.upb.nrs.sl.RoundEven).toBigDecimal.toString + " ")
                    if (idx % 3 == 2) {
                        writer.write("\n")
                    }    
                }

                writer.close()
            }
        }
    }
}


class MatrixMultiplyPosit5X5 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrices with integer elements"


    it should "multiply 2 matrices" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.Posit(None, 2 , 32), () => new ro.upb.nrs.hgl.performance.Posit(None, 2, 32), 5)) { c =>
            
            def initMatrix(ncols: Int, nrows: Int) : Array[Int] = {
                Array.tabulate(ncols * nrows) { x => x + 1 }
            }

            for (fileNo <- 0 until 50) {

                val a = scala.io.Source.fromFile(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/tests5X5/test$fileNo.txt").getLines().toArray.filter(_ != "\n")
                val b = a.slice(0, 11)
                
                var matrix1 = Seq("")
                var matrix2 = Seq("")

                for (line <- 0 until (b.size / 2)) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size) {
                        matrix1 ++= Seq(tokens(i))
                    }
                }

                var matrix1filtered = matrix1.slice(1, 26).map(x => "b" + (ro.upb.nrs.sl.Posit(x.toDouble, 2, 32, ro.upb.nrs.sl.RoundEven).toBinaryString))

                for (line <- (b.size / 2 + 1) until b.size) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size){
                        matrix2 ++= Seq(tokens(i))
                    }
                }

                var matrix2filtered = matrix2.slice(1, 26).map(x => "b" + (ro.upb.nrs.sl.Posit(x.toDouble, 2, 32, ro.upb.nrs.sl.RoundEven).toBinaryString))
                
                /*for (i <- 0 until 5 * 5) {
                    println(matrix1filtered(i), matrix2filtered(i))
                }*/

                for (idx <- 0 until 5 * 5) {
                    c.io.matrix1(idx).value.poke(matrix1filtered(idx).U)
                    c.io.matrix2(idx).value.poke(matrix2filtered(idx).U)
                }
                
                //val writer = new PrintWriter(new File(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/Posit32_2/test$fileNo.out"))

                c.clock.step()

                for (idx <- 0 until 5 * 5) {
                    val x = c.io.matrixRes(idx).value.peek()
                    val y = x.asBools.reverse
                    val z = y.map(v => v.litValue).foldLeft("")(_ + _)
                    //writer.write(ro.upb.nrs.sl.Posit(z, 2, 32, ro.upb.nrs.sl.RoundEven).toBigDecimal.toString + " ")
                    //if (idx % 5 == 4) {
                      //  writer.write("\n")
                    //}    
                }

                //writer.close()
            }
        }
    }
}

class MatrixMultiplyFixedPoint5X5 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrices with integer elements"


    it should "multiply 2 matrices" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.FixedPointQ(None, 32, 17), () => new ro.upb.nrs.hgl.performance.FixedPointQ(None, 32, 17), 5)) { c =>

        for (fileNo <- 0 until 50) {

                val a = scala.io.Source.fromFile(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/tests5X5/test$fileNo.txt").getLines().toArray.filter(_ != "\n")
                val b = a.slice(0, 11)
                
                var matrix1 = Seq("")
                var matrix2 = Seq("")

                for (line <- 0 until (b.size / 2)) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size) {
                        matrix1 ++= Seq(tokens(i))
                    }
                }

                var matrix1filtered = matrix1.slice(1, 26)

                for (line <- (b.size / 2 + 1) until b.size) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size){
                        matrix2 ++= Seq(tokens(i))
                    }
                }

                var matrix2filtered = matrix2.slice(1, 26)

                for (idx <- 0 until 5 * 5) {
                    c.io.matrix1(idx).value.poke(FixedPoint.fromBigDecimal(BigDecimal(matrix1filtered(idx)), 32.W, 17.BP))
                    c.io.matrix2(idx).value.poke(FixedPoint.fromBigDecimal(BigDecimal(matrix2filtered(idx)), 32.W, 17.BP))
                }
                
                val writer = new PrintWriter(new File(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/FixedPoint15_17/test$fileNo.out"))

                c.clock.step()

                for (idx <- 0 until 5 * 5) {
                    val x = c.io.matrixRes(idx).value.peek()
                    writer.write(FixedPoint.toBigDecimal(x.litValue, 17).toString + " ")
                    if (idx % 5 == 4) {
                        writer.write("\n")
                    }
                }

                writer.close()
            }
        }
    }
}


class MatrixMultiplyFractional5X5 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Matrices with fracional elements"


    it should "multiply 2 matrices with fractional elements" in {
        test(new MatrixMultiply(() => new ro.upb.nrs.hgl.performance.Fractional(None, 16, 15), () => new ro.upb.nrs.hgl.performance.Fractional(None, 16, 15), 5)) { c =>
            
            def initMatrix(ncols: Int, nrows: Int) : Array[Int] = {
                Array.tabulate(ncols * nrows) { x => x + 1 }
            }

            for (fileNo <- 0 until 1) {

                /*val a = scala.io.Source.fromFile(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/tests5X5/test$fileNo.txt").getLines().toArray.filter(_ != "\n")
                val b = a.slice(0, 11)
                
                var matrix1 = Seq("")
                var matrix2 = Seq("")

                for (line <- 0 until (b.size / 2)) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size) {
                        matrix1 ++= Seq(tokens(i))
                    }
                }

                var matrix1filtered = matrix1.slice(1, 26).map(x => "b" + (ro.upb.nrs.sl.FractionalNumber(x.toDouble, 16, 15, ro.upb.nrs.sl.RoundEven).toBinaryString))

                for (line <- (b.size / 2 + 1) until b.size) {
                    val tokens = b(line).split(" ")
                    for (i <- 0 until tokens.size){
                        matrix2 ++= Seq(tokens(i))
                    }
                }

                var matrix2filtered = matrix2.slice(1, 26).map(x => "b" + (ro.upb.nrs.sl.FractionalNumber(x.toDouble, 16, 15, ro.upb.nrs.sl.RoundEven).toBinaryString))*/

                for (idx <- 0 until 5 * 5) {
                    c.io.matrix1(idx).value.poke(0.U)
                    c.io.matrix2(idx).value.poke(0.U)
                }
                
                //val writer = new PrintWriter(new File(System.getProperty("user.dir") + s"/src/test/scala/ro/upb/nrs/hl/HNumberRepresentationSystem/Fractional16_15/test$fileNo.out"))

                c.clock.step()

                for (idx <- 0 until 5 * 5) {
                    val x = c.io.matrixRes(idx).value.peek()
                    val y = x.asBools.reverse
                    val z = y.map(v => v.litValue).foldLeft("")(_ + _)
                    //println(z, z.length)
                    //writer.write(ro.upb.nrs.sl.FractionalNumber(z, 16, 15, ro.upb.nrs.sl.RoundEven).toBigDecimal.toString + " ")
                    //if (idx % 5 == 4) {
                      //  writer.write("\n")
                    //}    
                }

                //writer.close()
            }
        }
    }
}