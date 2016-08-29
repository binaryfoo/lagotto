package io.github.binaryfoo.lagotto

object Iso8583 {

  def invertMTI(mti: String): String = {
    if (mti.length == 4 || mti.length == 2) {
      if (isResponseMTI(mti))
        toRequestMti(mti)
      else
        toResponseMti(mti)
    } else {
      mti
    }
  }

  def isResponseMTI(mti: String): Boolean = {
    mti.length match {
      case 4 => mti(2).toInt % 2 == 1
      case 2 => !HsmCommands.Commands.contains(mti)
    }
  }

  def isRequestMTI(mti: String): Boolean = {
    mti.length match {
      case 4 => mti(2).toInt % 2 == 0
      case 2 => HsmCommands.Commands.contains(mti)
    }
  }

  def toRequestMti(mti: String): String = {
    val directionIdx = directionIndex(mti)
    val characters = mti.toCharArray
    characters(directionIdx) = (mti(directionIdx).toInt - 1).toChar
    new String(characters)
  }

  def toResponseMti(mti: String): String = {
    val directionIdx = directionIndex(mti)
    val characters = mti.toCharArray
    characters(directionIdx) = (mti(directionIdx).toInt + 1).toChar
    new String(characters)
  }

  @inline
  private def directionIndex(mti: String) = mti.length match {
    case 4 => 2
    case 2 => 1
  }
}
