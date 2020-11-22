/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension._DimensionType

import scala.annotation.tailrec

/**
 * @author gweinbach on 15/11/2020
 * @since 0.1.0
 */
object Dimension {

  import Numeric.Implicits._
  
  private[Dimension] type _DimensionType = Double
  
  private[Dimension] val _DimensionNumeric: Numeric[_DimensionType] = summon[Numeric[_DimensionType]]

  private[Dimension] object _DimensionType {
    val Zero: _DimensionType = 0.0
    val Epsilon: _DimensionType = Double.MinPositiveValue
  }
  
  enum Geometry {

    case Toric

    case Bounded

    private[Dimension] def normalizePosition(value: _DimensionType,
                                             boundary: SizeValue): _DimensionType =
      this match {
        case Toric =>
          boundary.remainder(value)

        case Bounded =>
          boundary.bounce(value)
      }

    private[Dimension] def normalizeSize(value: _DimensionType,
                                         origin: PositionValue,
                                         boundary: SizeValue): _DimensionType =
      this match {
        case Toric =>
          value.abs // size is not limited in a Toric Geometry

        case Bounded =>
          // to ensure position is normalized within given boundary
          val normalizedOrigin = normalizePosition(origin, boundary)                   
          _DimensionNumeric.min(value.abs, (boundary - normalizedOrigin).abs) // size might schrink in a Bounded Geometry
      }
      
    private[Dimension] def pushPosition(position: _DimensionType,
                                        pushing: _DimensionType,
                                        boundary: SizeValue): _DimensionType =
      this match {
        case Toric =>
          normalizePosition(position + pushing, boundary)

        case Bounded =>
          boundary.bind(position + pushing)
      }
  }


  opaque type SizeValue = _DimensionType

  object SizeValue {

    val Zero: SizeValue = _DimensionType.Zero

    def apply(size: _DimensionType): SizeValue = size.abs

    def apply(position1: PositionValue,
              position2: PositionValue): SizeValue =
      SizeValue(position2 - position1)
      
    def apply(size: _DimensionType,
              fromPosition: PositionValue,
              withinBoundary: SizeValue,
              usingGeometry: Geometry): SizeValue =
      usingGeometry.normalizeSize(size, fromPosition, withinBoundary)
  }


  import scala.math.Numeric.IntIsIntegral

  extension(sizeValue: SizeValue) {

    def isNull: Boolean =
      sizeValue == SizeValue.Zero

    def isNotNull: Boolean =
      sizeValue != SizeValue.Zero

    def isInBounds(position: PositionValue): Boolean =
      (position >= PositionValue.Zero) && (position < sizeValue)

    def isOutOfBounds(position: PositionValue): Boolean =
      !isInBounds(position)

    @tailrec
    private[Dimension] def remainder(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        _DimensionType.Zero
      }
      else if (dimensionValue < _DimensionType.Zero) {
        remainder(-dimensionValue)
      }
      else {
        dimensionValue % sizeValue
      }
    
    @tailrec
    private[Dimension] def bounce(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        _DimensionType.Zero
      }
      else if (dimensionValue < _DimensionType.Zero) {
        bounce(-dimensionValue)
      }
      else if (dimensionValue >= sizeValue) {
        val remainder = (dimensionValue - sizeValue) % sizeValue
        if (((dimensionValue - sizeValue) / sizeValue).floor.toInt % 2 == 0) {
          sizeValue - remainder
        }
        else {
          remainder
        }
      }
      else {
        dimensionValue
      }
      
    private[Dimension] def bind(dimensionValue: _DimensionType): _DimensionType =
      if (isNull) {
        _DimensionType.Zero
      }
      else if (dimensionValue < _DimensionType.Zero) {
        _DimensionType.Zero
      }
      else if (dimensionValue >= sizeValue) {
        sizeValue - _DimensionType.Epsilon
      }
      else {
        dimensionValue
      }
  }

  // Position
  opaque type PositionValue = _DimensionType

  object PositionValue {

    val Zero: PositionValue = _DimensionType.Zero

    def apply(position: _DimensionType,
              withinBoundary: SizeValue,
              usingGeometry: Geometry): PositionValue =
      usingGeometry.normalizePosition(position, withinBoundary)

  }

  extension(position: PositionValue) {
    
    def push(pushed: SizeValue)
            (using boundary: SizeValue)
            (using geometry: Geometry): PositionValue =
      geometry.pushPosition(position, pushed, boundary)

    def move(movement: MovementValue)
            (using boundary: SizeValue)
            (using geometry: Geometry): PositionValue =
      movement.from(position)(using boundary)(using geometry)
      
    def to(otherPosition: PositionValue): MovementValue =
      MovementValue(position, otherPosition)

    def compare(otherPosition: PositionValue): Int =
      summon[Ordering[Double]].compare(position, otherPosition)
  }

  given Ordering[PositionValue] = 
    new Ordering[PositionValue] {
      override def compare(x: PositionValue,
                           y: PositionValue): Int = x.compare(y)
    }

  
  // Movement
  opaque type MovementValue = _DimensionType

  object MovementValue {
    val Zero: MovementValue = _DimensionType.Zero
    
    def apply(movement: _DimensionType): MovementValue =
      movement

    def apply(position1: PositionValue,
              position2: PositionValue): MovementValue = 
      MovementValue(position2 - position1)
  }

  extension(movement: MovementValue) {

    def from(position: PositionValue)
            (using boundary: SizeValue)
            (using geometry: Geometry): PositionValue =
      PositionValue(position + movement, boundary, geometry)
      
    def accelerate(to: AccelerationValue): MovementValue =
      to(movement)
  }

  
  // Acceleration
  opaque type AccelerationValue = _DimensionType
  
  object AccelerationValue {
    val Zero: AccelerationValue = _DimensionType.Zero

    def apply(acceleration: _DimensionType): AccelerationValue =
      acceleration
  }

  extension(acceleration: AccelerationValue) {

    def apply(movement: MovementValue): MovementValue =
      MovementValue(movement + acceleration)
  }

  
  /** Utilities methods added to [[_DimensionType]] */
  extension(dimensionValue: _DimensionType) {

    def size: SizeValue =
      SizeValue(dimensionValue)

    def position(using boundary: SizeValue)(using geometry: Geometry): PositionValue =
      PositionValue(dimensionValue, boundary, geometry)
      
    def movement: MovementValue =
      MovementValue(dimensionValue)
      
    def acceleration: AccelerationValue =
      AccelerationValue(dimensionValue)
  }
  
  
  case class Solid private(position: PositionValue,
                           size: SizeValue)
  
  object Solid {
    
    def apply(position: PositionValue, 
              size: SizeValue)
             (using boundary: SizeValue)
             (using geometry: Geometry): Solid = {
      
      // to be sure that position is normalized within the same boundaries/geometry as the size
      val normalizedPosition = PositionValue(position, boundary, geometry)
      
      // calculates the maximum possible size from given position in the boundary using given geometry
      val normalizedSize = SizeValue(size, normalizedPosition, boundary, geometry)
      
      // might move the position if size was constrained by boundary
      val actualPosition = normalizedPosition.push(normalizedSize - size)
      
      // the second normalization takes into account the fact that boundary can be smaller than size
      // and therefore, final size can be smaller than given size
      val finalSize = SizeValue(geometry.normalizeSize(size, actualPosition, boundary))
      
      Solid(actualPosition, finalSize)
    }
  }
}