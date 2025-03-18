package sqlplus.example

import sqlplus.helper.ImplicitConversions._
import org.apache.spark.sql.SparkSession
import org.apache.spark.SparkConf

object SparkSQLPlusExample {
	def main(args: Array[String]): Unit = {
		val conf = new SparkConf()
		conf.setAppName("SparkSQLPlusExample")
		conf.setMaster("local[*]")
		val spark = SparkSession.builder.config(conf).getOrCreate()

		val v1 = spark.sparkContext.textFile("examples/data/graph.dat").map(row => {
			val f = row.split(",")
			Array[Any](f(0).toInt, f(1).toInt)
		}).persist()
		v1.count()
		val longLessThan = (x: Long, y: Long) => x < y

		val v2 = v1.keyBy(x => x(1).asInstanceOf[Int])
		val v3 = v2.groupBy()
		val v4 = v3.sortValuesWith[Long,Long,Long,Long](2, (x: Long, y: Long) => longLessThan(x, y)).persist()
		val v5 = v4.extractFieldInHeadElement(2)
		val v6 = v1.keyBy(x => x(0).asInstanceOf[Int])
		val v7 = v6.appendExtraColumn(v5)
		val v8 = v7.reKeyBy(x => x(1).asInstanceOf[Int])
		val v9 = v8.groupBy()
		val v10 = v9.sortValuesWith[Long,Long,Long,Long](2, (x: Long, y: Long) => longLessThan(x, y)).persist()
		val v11 = v10.extractFieldInHeadElement(2)
		val v12 = v1.keyBy(x => x(0).asInstanceOf[Int])
		val v13 = v12.appendExtraColumn(v11)
		val v14 = v13.filter(x => longLessThan(x._2(3).asInstanceOf[Long], x._2(2).asInstanceOf[Long]))

		val v15 = v14.map(t => (t._2(0).asInstanceOf[Int], Array(t._2(1),t._2(2))))
		val v16 = v15.enumerateWithOneComparison[Long,Long,Long,Long,Int](v10, 1, 2, (x: Long, y: Long) => longLessThan(y, x), Array(0,1), Array(1), (l, r) => (r(0).asInstanceOf[Int]))
		val v17 = v16.enumerateWithOneComparison[Long,Long,Long,Long,Int](v4, 1, 2, (x: Long, y: Long) => longLessThan(y, x), Array(0,1,2), Array(0,1,2))

		val v18 = v17.map(x => Array(x._2(0).toString, x._2(1).toString, x._2(2).toString, x._2(3).toString, x._2(4).toString, x._2(5).toString))
		v18.take(20).map(r => r.mkString(",")).foreach(println)
		println("only showing top 20 rows")

		spark.close()
	}
}
