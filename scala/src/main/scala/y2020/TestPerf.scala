package y2020

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.SeqHasAsJava

object TestPerf {
  def main(args: Array[String]): Unit = {
    println("First run is warmup")
    run()

    println("------------------------------")
    run()
  }

  def run(): Unit = {
    val testSize = 1000000

    {
      val l = Range(0, testSize).to(Vector)
      val start = System.nanoTime()
      val res = l
        .map(x => x + 1)
        .sum
      println(res)
      println("(baseline, just one map) vector completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }

    {
      val ll = Range(0, testSize).to(LazyList)
      val start = System.nanoTime()
      val res = ll
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .sum
      println(res)
      println("lazy list completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }

    {
      val l = Range(0, testSize).to(Stream)
      val start = System.nanoTime()
      val res = l
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .sum
      println(res)
      println("stream completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }

    {
      val l = Range(0, testSize).to(List)
      val start = System.nanoTime()
      val res2 = l
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .sum
      println(res2)
      println("scala list completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }

    {
      val l = Range(0, testSize).to(Vector)
      val start = System.nanoTime()
      val res2 = l
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .sum
      println(res2)
      println("vector completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }

    {
      val l = Range(0, testSize).to(ArrayBuffer)
      val start = System.nanoTime()
      val res2 = l
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .sum
      println(res2)
      println("arraybuffer completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }

    {
      val l = Range(0, testSize).to(ArrayBuffer)
      val start = System.nanoTime()
      val res2 = l
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .mapInPlace(x => x + 1)
        .sum
      println(res2)
      println("arraybuffer mapInPlace completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }

    {
      val l = new java.util.ArrayList[Int](Range(0, testSize).toList.asJava)
      val start = System.nanoTime()
      val res = l.stream()
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .map(x => x + 1)
        .reduce((x,y) => x+y)
        .get()
      println(res)
      println("java stream completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }

    {
      val l = Range(0, testSize).to(Vector)
      val start = System.nanoTime()
      val res = l
        .map(((x: Int) => x + 1)
        .andThen((x: Int) => x + 1)
        .andThen((x: Int) => x + 1)
        .andThen((x: Int) => x + 1)
        .andThen((x: Int) => x + 1)
        .andThen((x: Int) => x + 1)
        .andThen((x: Int) => x + 1)
        .andThen((x: Int) => x + 1)
        .andThen((x: Int) => x + 1)
        .andThen((x: Int) => x + 1))
        .sum
      println(res)
      println("vector with composed functions completed in "+(System.nanoTime() - start) / 1e9+" seconds")
    }
  }
}
