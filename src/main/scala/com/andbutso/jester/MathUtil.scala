package com.andbutso.jester

object MathUtil {
	def truncate(d: Double) = {
		Math.round(d * 100.0) / 100.0
	}
}
