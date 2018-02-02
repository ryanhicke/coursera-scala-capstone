package observatory.utils

import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object SparkUtils {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  lazy val conf = new SparkConf().setAppName("observatory").setMaster("local[4]")
  lazy val spark = new SparkContext(conf)

  def loadResourceAsTextFile(path: String): RDD[String] = spark.textFile(getClass.getResource(path).getPath)
}
