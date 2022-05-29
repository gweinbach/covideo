/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.*

/**
 * @since 0.2.0
 * @author gweinbach on 29/05/2022
 */
object Screen:

  opaque type Pixel = Int

  extension (pixel: Pixel)

    def +(other: Pixel): Pixel =
      pixel + other

    def -(other: Pixel): Pixel =
      pixel - other

    //  def size(viewScale: Double): SizeValue =
    //    (pixel size).zoom(1.0 / viewScale)(using DoubleIsFractional)

    def asInt: Int =
      pixel

  extension [N: Numeric](n: N)
    def px: Pixel = summon[Numeric[N]].toInt(n)

  case class ScreenDimension(width: Pixel,
                             height: Pixel):
    def withMargin(margin: Margin): ScreenDimension =
      ScreenDimension(
        width + margin.left + margin.right,
        height + margin.top + margin.bottom
      )

  object ScreenDimension:
    val Empty = ScreenDimension(0 px, 0 px)

  case class ScreenPosition(x: Pixel,
                            y: Pixel)

  object ScreenPosition:
    val TopLeft = ScreenPosition(0 px, 0 px)

  case class Margin(top: Pixel = 0 px,
                    left: Pixel = 0 px,
                    bottom: Pixel = 0 px,
                    right: Pixel = 0 px)
