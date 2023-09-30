//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using file util/NumUtils.scala
//> using resourceDir inputs

import util.NumUtils.divisors
import util.ResourceUtils.readResourceLines

import scala.collection.mutable
import scala.annotation.tailrec

object Day21 {
  // This is the only instruction in the input program that interacts with register 0!
  private[this] final val Reg0InstructionId = 28

  private[this] case class Instruction(opcode: String, op1: Int, op2: Int, op3: Int)

  def main(args: Array[String]): Unit = {
    val input: Array[String] = readResourceLines("day21.txt")

    val ipRegister = input.head.split(" ").last.toInt
    val instructions = input
      .drop(1)
      .map(_.split(" ").toList)
      .map {
        case List(opcode, op1, op2, op3) => Instruction(opcode, op1.toInt, op2.toInt, op3.toInt)
        case _                           => throw new Error("Invalid input line!")
      }

    def evalInstruction(instruction: Instruction, registers: Array[Int]): Int = instruction.opcode match {
      case "addi" => registers(instruction.op1) + instruction.op2
      case "addr" => registers(instruction.op1) + registers(instruction.op2)
      case "seti" => instruction.op1
      case "setr" => registers(instruction.op1)
      case "mulr" => registers(instruction.op1) * registers(instruction.op2)
      case "muli" => registers(instruction.op1) * instruction.op2
      case "eqrr" => if (registers(instruction.op1) == registers(instruction.op2)) 1 else 0
      case "eqir" => if (instruction.op1 == registers(instruction.op2)) 1 else 0
      case "eqri" => if (registers(instruction.op1) == instruction.op2) 1 else 0
      case "gtrr" => if (registers(instruction.op1) > registers(instruction.op2)) 1 else 0
      case "gtir" => if (instruction.op1 > registers(instruction.op2)) 1 else 0
      case "gtri" => if (registers(instruction.op1) > instruction.op2) 1 else 0
      case "banr" => registers(instruction.op1) & registers(instruction.op2)
      case "bani" => registers(instruction.op1) & instruction.op2
      case "borr" => registers(instruction.op1) | registers(instruction.op2)
      case "bori" => registers(instruction.op1) | instruction.op2
    }

    def runProgram(registers: Array[Int], ip: Int = 0): Int = {
      if (ip >= instructions.length) throw new Error("The program exited!")
      else {
        registers(ipRegister) = ip
        val instruction = instructions(ip)

        if (ip == Reg0InstructionId) {
          return registers(instruction.op1)
        }

        registers(instruction.op3) = evalInstruction(instruction, registers)
        runProgram(registers, registers(ipRegister) + 1)
      }
    }

    val part1 = runProgram(Array(0, 0, 0, 0, 0, 0))
    println(s"Part 1: $part1")
  }
}
