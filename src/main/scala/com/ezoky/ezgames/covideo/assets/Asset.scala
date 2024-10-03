package com.ezoky.ezgames.covideo.assets

import java.net.URL


case class Asset(fileName: String):
  val url: URL = getClass().getResource(fileName)

object Asset:

  val SmileySunglasses = Asset("smiley-sunglasses-33x33.png")
  val SmileySick = Asset("sick-emoji-33x33.png")
  