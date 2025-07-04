package main.scala.ro.upb

import scala.collection.mutable.ArrayBuffer

import chisel3._
import firrtl.options.TargetDirAnnotation
import circt.stage.ChiselStage
import ro.upb.nrs.hgl._
import chisel3.stage.ChiselGeneratorAnnotation

object VerilogGenerator extends App {

    if (args.length != 1) {
        println("Usage: mill verilog target")
        println("Examples:")
        println("  mill verilog GFPU")
        System.exit(1)
    }

    val targetName = args(0)

    val moduleGen: () => RawModule = targetName match {
        case "GFPU" => 
            () => {
                val nrss : List[FloatingPointTrait] = List(
                    FixedFloatingPoint(4, 3, RoundEven, false),
                    IEEE754(4, 3, RoundEven, false)
                )
                new GFPU(32, nrss, softwareDebug = false)
            }
        
        case _ => 
            throw new IllegalArgumentException(s"Unknown module: $targetName")
    }

    val outputDir = s"generated-verilog/$targetName"
  
    (new ChiselStage).execute(
        Array("--target", "verilog", "--target-dir", outputDir),
        Seq(ChiselGeneratorAnnotation(moduleGen))
    )
    
    println(s"Verilog generated for $targetName in $outputDir")
}