/*
 * @author gweinbach on 19/06/2022 04:33
 * @since 0.2.0
 */

package com.ezoky.ez3d

import spire.math.Numeric

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

    def /[N: Numeric](other: N): Pixel =
      summon[Numeric[N]].div(pixel, other) px

    def *[N: Numeric](other: N): Pixel =
      summon[Numeric[N]].times(pixel, other) px

    def asInt: Int =
      pixel

  extension[N: Numeric] (n: N)
    def px: Pixel = summon[Numeric[N]].toInt(n)

  given [N: Numeric]: Conversion[Pixel, N] with
    def apply(pixel: Pixel): N = summon[Numeric[N]].fromInt(pixel.asInt)

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

  
  type ScreenVertices = Iterable[ScreenVertex]

  case class ScreenVertex(s: ScreenPosition,
                          t: ScreenPosition)
  
  case class ScreenShape(vertices: ScreenVertices)
