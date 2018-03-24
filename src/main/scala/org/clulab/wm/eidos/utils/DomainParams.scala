package org.clulab.wm.eidos.utils

import org.clulab.wm.eidos.Aliases.Param

class DomainParams(domainParamKBFile: String) {
  protected val domainParamValues: Map[Param, Map[String, Double]] =
      FileUtils.getCommentedLinesFromSource(Sourcer.sourceFromResource(domainParamKBFile))
          .map { line => // line = [param]\t[variable]\t[value] => e.g. "rainfall  mean  30.5"
            val fields = line.split("\t")
            val param = fields(0)
            val var_values = fields.tail.map { var_value =>
              val tmp = var_value.split(":")
              val variable = tmp(0)
              val value = tmp(1).toDouble // Assuming the value of the variable is a double. TODO: Change this appropriately
              variable -> value
            }.toMap
            (param -> var_values)
          }.toMap
  
  def get(key: String) = domainParamValues.get(key)
  
  def keys = domainParamValues.keys
  
  override def toString = domainParamValues.toString
}

object DomainParams {
  val DEFAULT_DOMAIN_PARAM: String = "DEFAULT"
  val PARAM_MEAN: String = "mean"
  val PARAM_STDEV: String = "stdev"

  def apply(domainParamKBFile: String) =
      new DomainParams(domainParamKBFile)
}
