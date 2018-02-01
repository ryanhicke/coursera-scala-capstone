package observatory.utils

import observatory.fixtures.BaseSpec

class SparkUtilsSpec extends BaseSpec {
  "SparkUtils" should "load a text RDD from a resource" in {
    // this file is just one sentence repeated on 10 lines
    val rdd = SparkUtils.loadResourceAsTextFile("/quick-brown-fox")
    val collected = rdd.collect()
    collected.length should be(10)
    collected.foreach(row => row should be("The quick brown fox jumps over the lazy dog"))
  }
}
