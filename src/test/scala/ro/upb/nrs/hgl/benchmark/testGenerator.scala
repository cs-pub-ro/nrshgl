import java.io._

class testGenerator() {
    def generator(noOfFiles: Int): Unit = {
        val r = new scala.util.Random(1000)
        r.setSeed(1000L)
        for (i <- 0 until noOfFiles) {
            val writer = new PrintWriter(new File(s"tests5X5/test$i.txt"))
            for (j <- 0 until 50) {
                val x = r.nextInt()
                val y = r.nextInt()
                writer.write((x.toDouble / y.toDouble).toString + " ")
                if (j % 5 == 4) {
                    writer.write("\n")
                }
                if (j % 25 == 24) {
                    writer.write("\n")
                }
            }
            writer.close()
        }
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        val task: testGenerator = new testGenerator()
        task.generator(50)
    }
}