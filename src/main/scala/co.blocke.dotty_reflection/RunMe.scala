package co.blocke.dotty_reflection

import java.io._ 
import java.nio._ 

case class Foom(mame:String, age:Int)

object RunMe extends App:

  println(RType.of[Foom])
  
/*

  // NOTES:  2-4x speed improvement doing our own byte serialization over Java implemention

  val stuff = Array("Gregory", "William", "Zoller")
  val ssr = StringReadWriter()
  val asr = ArrayReadWriter(ssr)

  val b = java.nio.ByteBuffer.allocate(2045)
  val b2 = java.nio.ByteBuffer.allocate(2048)

  val now = System.currentTimeMillis
  for( i <- 0 to 1000000)
    oneRun()
  val later = System.currentTimeMillis
  println("RType serialization: "+(later-now))

  val now2 = System.currentTimeMillis
  for( i <- 0 to 1000000)
    twoRun()
  val later2 = System.currentTimeMillis
  println("Java serialization : "+(later2-now2))

  def oneRun() =
    b.rewind
    b2.rewind
    asr.write(b, stuff)
    val str = new String(b.array)
    b2.put( str.getBytes )
    b2.flip
    asr.read(b2)

  def twoRun() =
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream( baos )
    oos.writeObject( stuff )
    oos.close()
    val str = java.util.Base64.getEncoder().encodeToString(baos.toByteArray())

    val data = java.util.Base64.getDecoder().decode( str )
    val ois  = new ObjectInputStream( new ByteArrayInputStream( data ) )
    val o    = ois.readObject()
    ois.close()
    o.asInstanceOf[Array[String]]
    */
    
  println("Done.")
  