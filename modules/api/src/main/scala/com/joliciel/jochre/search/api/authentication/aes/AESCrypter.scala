package com.joliciel.jochre.search.api.authentication.aes

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

case class AESCrypter(password: String) {
  private val algorithmName = "AES"
  private val cipherName = "AES/ECB/PKCS5Padding"
  private val passwordSize = 32

  val passwordBytes = password.getBytes("UTF-8")
  if (passwordBytes.length > passwordSize) {
    new RuntimeException("Please choose a password that results in a 32 length byte array")
  }

  def encrypt(str: String): Array[Byte] = {
    val bytes = str.getBytes("UTF-8")
    val secretKey = new SecretKeySpec(passwordBytes, algorithmName)
    val encipher = Cipher.getInstance(cipherName)
    encipher.init(Cipher.ENCRYPT_MODE, secretKey)
    encipher.doFinal(bytes)
  }

  def decrypt(bytes: Array[Byte]): String = {
    val secretKey = new SecretKeySpec(passwordBytes, algorithmName)
    val encipher = Cipher.getInstance(cipherName)
    encipher.init(Cipher.DECRYPT_MODE, secretKey)
    new String(encipher.doFinal(bytes), "UTF-8")
  }
}
