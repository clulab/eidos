package ai.lum.eidos.restapp.models.utils

import java.net.URLEncoder
import java.util.UUID

// This code is borrowed from the Swagger output from the rest api.
object QueryString {

  def escape(value: String): String = {
    URLEncoder.encode(value, "utf-8").replaceAllLiterally("%20", "+")
    //.replaceAll("\\+", "%20") // This must be wrong.
  }

  def escape(values: List[String]): String = {
    values.map(escape).mkString(",")
  }

  def escape(value: Long): String = value.toString

  def escape(value: Double): String = value.toString

  def escape(value: Float): String = value.toString

  def escape(value: UUID): String = value.toString

  def escape(queryParams: Map[String, String]): String = {
    queryParams
        .filter(keyAndValue => keyAndValue._2 != null)
        .map(keyAndValue => escape(keyAndValue._1) + "=" + escape(keyAndValue._2))
        .mkString("?", "&", "")
  }
}
