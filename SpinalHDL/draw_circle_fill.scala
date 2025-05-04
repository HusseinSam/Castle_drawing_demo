package castledrawing
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
case class draw_circle_fill(CORDW: Int) extends Component {noIoPrefix()
  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val start   = in Bool()
    val oe      = in Bool()
    val x0      = in SInt(CORDW bits)
    val y0      = in SInt(CORDW bits)
    val r0      = in SInt(CORDW bits)
    val x       = out SInt(CORDW bits)
    val y       = out SInt(CORDW bits)
    val drawing = out Bool()
    val busy    = out Bool()
    val done    = out Bool()
  }
  val myClockDomain = ClockDomain(clock = io.clk, reset = io.rst, config = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = ASYNC))
  val myArea = new ClockingArea(myClockDomain) {
    // Internal signals
    val xa = Reg(SInt(CORDW bits)) init (0);
    val ya = Reg(SInt(CORDW bits)) init (0)
    val err = Reg(SInt((CORDW + 2) bits)) init (0);
    val err_tmp = Reg(SInt((CORDW + 2) bits)) init (0)
    // Horizontal line coordinates
    val lx0 = Reg(SInt(CORDW bits)) init (0);
    val lx1 = Reg(SInt(CORDW bits)) init (0)
    val line_start = Bool();
    val line_done = Bool()
    val yReg = Reg(SInt(CORDW bits)) init (0);
    io.y := yReg
    val initDelayCounter = Reg(UInt(1 bits)) init (0)
    val fsm = new StateMachine { // State machine definition
      io.done := False
      val IDLE = makeInstantEntry();val CALC_Y = new State();val CALC_X = new State();val CORDS_DOWN = new State();val LINE_DOWN = new State();val CORDS_UP = new State();val LINE_UP = new State()
      IDLE
        .whenIsActive {
          io.done := False
          io.busy := False
          line_start := False
          when(io.start) {
            xa := -(io.r0)
            ya := 0
            err := 2 - (2 * io.r0).resize(CORDW + 2)
            io.busy := True
            goto(CORDS_DOWN)
          }
        }
      CALC_Y
        .whenIsActive {
          io.busy := True
          line_start := False
          when(xa === 0) {

            io.done := True
            goto(IDLE)
          } otherwise {
            err_tmp := err
            when(err <= ya.resize(CORDW + 2)) {
              ya := ya + 1
              err := err + (2 * (ya + 1) + 1).resize(CORDW + 2)
            }
            goto(CALC_X)
          }
        }
      CALC_X
        .whenIsActive {
          io.busy := True
          line_start := False
          when(err_tmp > xa.resize(CORDW + 2) || err > ya.resize(CORDW + 2)) {
            xa := xa + 1
            err := err + (2 * (xa + 1) + 1).resize(CORDW + 2)
          }
          goto(CORDS_DOWN)
        }
      CORDS_DOWN
        .whenIsActive {
          io.busy := True
          yReg := io.y0 + ya; lx0 := io.x0 + xa; lx1 := io.x0 - xa
          line_start := False
          when(initDelayCounter === 1) {
            goto(LINE_DOWN)
          } otherwise {initDelayCounter := initDelayCounter + 1}
        }
        .onExit {initDelayCounter := 0} // Reset the counter when exiting INIT}
      LINE_DOWN
        .whenIsActive {
          io.busy := True
          line_start := True
          when(line_done) {
            goto(CORDS_UP)
          }
        }
      CORDS_UP
        .whenIsActive {
          io.busy := True
          yReg := io.y0 - ya
          line_start := False
          goto(LINE_UP)
        }
      LINE_UP
        .whenIsActive {
          io.busy := True
          line_start := True
          when(line_done) {
            goto(CALC_Y)
          }
        }
    }
    val drawLineCircle = new draw_line_1d_circle(CORDW)  // Instantiate the draw_line_1d component
    drawLineCircle.io.clk := io.clk
    drawLineCircle.io.rst := io.rst
    drawLineCircle.io.start := line_start
    drawLineCircle.io.oe := io.oe
    drawLineCircle.io.x0 := lx0
    drawLineCircle.io.x1 := lx1
    io.x := drawLineCircle.io.x
    io.drawing := drawLineCircle.io.drawing
    line_done := drawLineCircle.io.done
  }
}
object draw_circle_fillVerilog extends App {Config.spinal.generateVerilog(new draw_circle_fill(16))}