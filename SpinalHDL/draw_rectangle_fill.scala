package castledrawing
import spinal.core._
import spinal.lib.fsm._
case class draw_rectangle_fill(CORDW: Int) extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val start = in Bool()
    val oe = in Bool()
    val x0 = in SInt (CORDW bits)
    val y0 = in SInt (CORDW bits)
    val x1 = in SInt (CORDW bits)
    val y1 = in SInt (CORDW bits)
    val x = out SInt (CORDW bits)
    val y = out SInt (CORDW bits)
    val drawing = out Bool()
    val busy = out Bool()
    val done = out Bool()
  }
  val myClockDomain = ClockDomain(clock = io.clk, reset = io.rst, config = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = ASYNC))
  val myArea = new ClockingArea(myClockDomain) {
    val line_id = Reg(SInt(CORDW bits)) init (0); val line_start = Bool(); val line_done = Bool()
    val y0s = Reg(SInt(CORDW bits)) init(0); val y1s = Reg(SInt(CORDW bits)) init(0)
    when(io.y0 > io.y1) {y0s := io.y1; y1s := io.y0} otherwise { y0s := io.y0; y1s := io.y1}
    val lx0 = Reg(SInt(CORDW bits)) init(0); val lx1 = Reg(SInt(CORDW bits)) init(0) ; val yReg = Reg(SInt(CORDW bits)) init(0); io.y := yReg
    val initDelayCounter = Reg(UInt(1 bits)) init(0)
    val fsm = new StateMachine {
      io.done := False
      val IDLE = makeInstantEntry(); val INIT = new State(); val DRAW = new State()
      IDLE
        .whenIsActive {
          line_start := False
          io.done := False
          io.busy := False
          when(io.start) {
            line_id := 0
            io.busy := True
            goto(INIT)
          }
        }
      INIT
        .whenIsActive {
          line_start := False; io.busy := True
          when(io.x0 > io.x1) {
            lx0 := io.x1 ; lx1 := io.x0
          } otherwise {
            lx0 := io.x0; lx1 := io.x1
          }
          yReg := (y0s + line_id).resized
          when(initDelayCounter === 1) {
            line_start := True
            goto(DRAW)
          } otherwise { initDelayCounter := initDelayCounter + 1 }
        }
        .onExit {initDelayCounter := 0 } // Reset the counter when exiting INIT
      DRAW
        .whenIsActive {
          io.busy := True
          line_start := False
          when(line_done) { when(yReg === y1s) {io.busy := False; io.done := True; goto(IDLE)} otherwise {line_id := line_id + 1; goto(INIT)
            }
          }
        }
      always {
        when(io.rst) {
          goto(IDLE)
          io.busy := False
          io.done := False
          line_id := 0
          line_start := False
          yReg := 0  // explicitly reset yReg
        }
      }
    }
    val drawLine1D = new draw_line_1d(CORDW)
    drawLine1D.io.clk := io.clk
    drawLine1D.io.rst := io.rst
    drawLine1D.io.start := line_start
    drawLine1D.io.oe := io.oe
    drawLine1D.io.x0 := lx0
    drawLine1D.io.x1 := lx1
    io.x := drawLine1D.io.x
    io.drawing := drawLine1D.io.drawing
    line_done := drawLine1D.io.done
  }
}
object draw_rectangle_fillVerilog extends App {Config.spinal.generateVerilog(new draw_rectangle_fill(16))}