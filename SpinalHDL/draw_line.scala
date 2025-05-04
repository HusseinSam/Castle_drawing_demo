package castledrawing
import spinal.core._
import spinal.lib.fsm._
case class draw_line(CORDW: Int) extends Component {noIoPrefix()
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val start = in Bool()
    val oe = in Bool()
    val x0 = in SInt(CORDW bits)
    val y0 = in SInt(CORDW bits)
    val x1 = in SInt(CORDW bits)
    val y1 = in SInt(CORDW bits)
    val x = out SInt(CORDW bits)
    val y = out SInt(CORDW bits)
    val drawing = out Bool()
    val busy = out Bool()
    val done = out Bool()
  }
  val myClockDomain = ClockDomain(clock = io.clk, reset = io.rst, config = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC))
  val myArea = new ClockingArea(myClockDomain) {
    val xReg = Reg(SInt(CORDW bits)) init (0); io.x := xReg //using xReg because in sv x is not a wire and this ensure a correct assignment
    val yReg = Reg(SInt(CORDW bits)) init (0); io.y := yReg
    val swap = io.y0 > io.y1 // Line properties
    val right = Reg(Bool())
    val xa = Mux(swap, io.x1, io.x0);val xb = Mux(swap, io.x0, io.x1)
    val ya = Mux(swap, io.y1, io.y0); val yb = Mux(swap, io.y0, io.y1)
    val x_end = Reg(SInt(CORDW bits)); val y_end = Reg(SInt(CORDW bits))
    val err = Reg(SInt((CORDW + 1) bits))// Error values
    val dx = SInt((CORDW + 1) bits); val dy = SInt((CORDW + 1) bits)
    val movx = (2 * err) >= dy;val movy = (2 * err) <= dx
    val dxReg = Reg(SInt((CORDW + 1) bits)) init (0); dx := dxReg
    val dyReg = Reg(SInt((CORDW + 1) bits)) init (0); dy := dyReg
    val doneFlag = Reg(Bool()) init(False); io.done := doneFlag
    val fsm = new StateMachine {
      io.drawing := False
      val IDLE = makeInstantEntry()
      val INIT_0 = new State()
      val INIT_1 = new State()
      val DRAW = new State()
      IDLE
        .whenIsActive {
          io.busy := False
          when(doneFlag) {doneFlag := False}
          when(io.start) {
            io.done := False; right := xa < xb; io.busy := False
            goto(INIT_0)
          }
        }
      INIT_0
        .whenIsActive {
          io.busy := True
          dxReg := (Mux(right, xb - xa, xa - xb)).resize(CORDW + 1); dyReg := (ya - yb).resize(CORDW + 1)
          goto(INIT_1)
        }
      INIT_1
        .whenIsActive {
          io.busy := True; err := dxReg + dyReg
          xReg := xa; yReg := ya; x_end := xb; y_end := yb
          goto(DRAW)
        }
      DRAW
        .whenIsActive {
          io.drawing := io.oe; io.busy := True
          when(io.oe) {
            when(xReg === x_end && yReg === y_end) {
              doneFlag := True; goto(IDLE)
            } otherwise {
              when(movx) {
                xReg := Mux(right, xReg + 1, xReg - 1)
                err := err + dyReg
              }
              when(movy) {
                yReg := yReg + 1 ; err := err + dxReg
              }
              when(movx && movy) {
                xReg := Mux(right, xReg + 1, xReg - 1)
                yReg := yReg + 1
                err := err + dyReg + dxReg
              }
            }
          }
        }
      always{
        when(io.rst) {
          goto(IDLE)
          io.busy := False ;io.done := False
        }
      }
    }
  }
}
object draw_lineVerilog extends App {Config.spinal.generateVerilog(new draw_line(16))}