package castledrawing
import spinal.core._
import java.nio.file.{Files, Paths}
object FileUtils { // Helper to read binary file into Array[BigInt] for Bits
  def FileToBigIntArray(path: String, width: Int, depth: Int): Array[BigInt] = {
    val bytes = Files.readAllBytes(Paths.get(path))
    val wordBytes = width / 8
    val data = Array.fill(depth)(BigInt(0))
    for (i <- 0 until Math.min(depth, bytes.length / wordBytes)) {
      val word = bytes.slice(i * wordBytes, (i + 1) * wordBytes)
      data(i) = BigInt(word.reverse)
    }
    data
  }
}
case class bram_sdp(WIDTH: Int = 8, DEPTH: Int = 256, INIT_F: String = "") extends Component {
  val ADDRW = log2Up(DEPTH)
  noIoPrefix()
  val io = new Bundle {
    val clk_write  = in Bool()                           // Write clock (port A)
    val clk_read   = in Bool()                           // Read clock (port B)
    val we         = in Bool()                           // Write enable (port A)
    val addr_write = in UInt(ADDRW bits)                 // Write address (port A)
    val addr_read  = in UInt(ADDRW bits)                 // Read address (port B)
    val data_in    = in Bits(WIDTH bits)                 // Data in (port A)
    val data_out   = out Bits(WIDTH bits)                // Data out (port B)
  }
  val mem = Mem(Bits(WIDTH bits), DEPTH)

  if (INIT_F.nonEmpty) {
    val initData = FileUtils.FileToBigIntArray(INIT_F, WIDTH, DEPTH)
    mem.init(initData.map(v => B(v, WIDTH bits)))
  }

  // Write port (port A)
  val writeArea = new ClockingArea(ClockDomain(io.clk_write)) {
    when(io.we) {
      mem.write(address = io.addr_write, data    = io.data_in)
    }
  }
  // Write port (port B)
  val readArea = new ClockingArea(ClockDomain(io.clk_read)) {
    io.data_out := mem.readAsync(io.addr_read)
  }
}
object bram_sdp_Verilog extends App { Config.spinal.generateVerilog(new bram_sdp()) }