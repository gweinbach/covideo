/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *  
 */

package com.ezoky.ezgames.covideo.component.swing

import com.ezoky.ezgames.covideo.component.swing.Assets
import org.junit.Assert.*
import org.junit.Test

/**
 * @author gweinbach on 04/01/2022
 * @since 0.2.0
 */
class AssetsTest {

  @Test def assetLoad: Unit = {
    val asset = Assets.SmileySunglasses
    assertNotNull(asset)
    assert(asset.getWidth > 0)
    assert(asset.getHeight > 0)
  }
}
