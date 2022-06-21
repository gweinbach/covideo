/*
 * @author gweinbach on 21/06/2022 23:09
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.Precision
import org.scalatest.flatspec.AnyFlatSpec

/**
 * @since 0.2.0
 * @author gweinbach on 21/06/2022
 */
class Model3DTest
  extends AnyFlatSpec :

  given Precision[Double] = Precision(1E-10)