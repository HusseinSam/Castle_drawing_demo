package castledrawing
import spinal.core._
import spinal.lib._
class top_castle(
                     CORDW: Int = 16,  // signed coordinate width (bits)
                     ) extends Component {
  noIoPrefix()
  val io = new Bundle {
    val clk_pix = in Bool() // clock
    val rst_pix = in Bool() // reset
    val sdl_sx, sdl_sy = out SInt (CORDW bits)
    val sdl_de = out Bool()
    val sdl_frame = out Bool() // drawing is complete (high for one tick)
    val sdl_r = out SInt (8 bits)
    val sdl_g = out SInt (8 bits)
    val sdl_b = out SInt (8 bits)
  }
  //custom clock domain
  val myClockDomain = ClockDomain(
    clock = io.clk_pix,
    reset = io.rst_pix,
    config = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC
    )
  )
  val myArea = new ClockingArea(myClockDomain) {
    val clk_sys = io.clk_pix
    val rst_sys = io.rst_pix
    // Display sync signals and coordinates
    val display = new display_480p(CORDW)
    display.io.clk_pix := io.clk_pix
    display.io.rst_pix := io.rst_pix
    val sx     = display.io.sx
    val sy     = display.io.sy
    val frame  = display.io.frame
    val de = display.io.de
    val line   = display.io.line
    // colour parameters
    val LIB_RES     = "../../../lib/res"
    val CHANW       = 4
    val COLRW       = 3 * CHANW
    val CIDXW       = 4
    val COLR_TRANS  = B"0010_0010_0011" // or U(0x223, 12 bits)
    val PAL_FILE    = "C:/Users/pc_2020/OneDrive - Technion/Desktop/SpinalTemplateSbt-master/SpinalTemplateSbt-master/hw/spinal/castledrawing/sweetie16_4b.mem"
    // framebuffer (FB)
    val FB_WIDTH    = 320
    val FB_HEIGHT   = 180
    val FB_SCALE    = 2
    val FB_OFFX     = 0
    val FB_OFFY     = 60
    val FB_PIXELS   = FB_WIDTH * FB_HEIGHT
    val FB_ADDRW    = log2Up(FB_PIXELS)
    val FB_DATAW    = CIDXW
    // pixel read and write addresses and colours
    val fb_addr_write = UInt(FB_ADDRW bits)
    val fb_addr_read  = Reg(UInt(FB_ADDRW bits)) init(0)
    val fb_colr_write = UInt(FB_DATAW bits)
    val fb_colr_read  = UInt(FB_DATAW bits)
    var fb_we = Bool()
    // framebuffer memory
    val bram_inst = bram_sdp(
      FB_DATAW,
      FB_PIXELS,
       ""
    )
    bram_inst.io.clk_write := clk_sys
    bram_inst.io.clk_read := clk_sys
    bram_inst.io.we := fb_we
    bram_inst.io.addr_write := fb_addr_write
    bram_inst.io.addr_read := fb_addr_read
    bram_inst.io.data_in := fb_colr_write.asBits.resize(FB_DATAW bits)
    fb_colr_read.asBits.resize(FB_DATAW bits) := bram_inst.io.data_out
    // Instantiate the xd modules for frame, line, and line0 flags
    val frame_sys  = Bool()
    val line_sys   = Bool()
    val line0_sys  = Bool()
    val xd_frame = new xd()
    val xd_line  = new xd()
    val xd_line0 = new xd()
    xd_frame.io.clk_src := io.clk_pix
    xd_frame.io.clk_dst := clk_sys
    xd_frame.io.flag_src := frame
    frame_sys := xd_frame.io.flag_dst
    xd_line.io.clk_src := io.clk_pix
    xd_line.io.clk_dst := clk_sys
    xd_line.io.flag_src := line
    line_sys := xd_line.io.flag_dst
    xd_line0.io.clk_src := io.clk_pix
    xd_line0.io.clk_dst := clk_sys
    xd_line0.io.flag_src := line && (sy === FB_OFFY)
    line0_sys := xd_line0.io.flag_dst
    //
    // draw in framebuffer
    //
    // reduce drawing speed to make process visible
    val FRAME_WAIT = 200
    val cntFrameWait = Reg(UInt(log2Up(FRAME_WAIT) bits)) init(0)
    val draw_oe = Reg(Bool()) init(False)
    draw_oe := False
    when(cntFrameWait =/= FRAME_WAIT - 1) {
      when(frame_sys) {
        cntFrameWait := cntFrameWait + 1
      }
    } elsewhen(line_sys) {
      draw_oe := True
    }
    // render shapes
    val drx  = Reg(SInt(CORDW bits)) // draw x coordinate
    val dry  = Reg(SInt(CORDW bits)) // draw y coordinate
    val drawing = Bool()
    val clip = Bool()
    val render = new render_castle()
    render.io.clk := clk_sys
    render.io.rst := rst_sys
    render.io.oe  := draw_oe
    render.io.start := frame_sys
    drx := render.io.x
    dry := render.io.y
    fb_colr_write := render.io.cidx
    drawing := render.io.drawing
    // calculate pixel address in framebuffer (three-cycle latency)
    val bitmapAddr = new bitmap_addr()
    bitmapAddr.io.clk := clk_sys
    bitmapAddr.io.bmpw := FB_WIDTH
    bitmapAddr.io.bmph := FB_HEIGHT
    bitmapAddr.io.x := drx
    bitmapAddr.io.y := dry
    bitmapAddr.io.offx := 0
    bitmapAddr.io.offy := 0
    fb_addr_write := bitmapAddr.io.addr
    clip := bitmapAddr.io.clip
    // delay write enable to match address calculation
    val LAT_ADDR = 3
    val fb_we_sr = Reg(Bits(LAT_ADDR bits)) init(0)// Shift register for write enable
    // Shift and insert `drawing`, reset on rst_sys
    when(rst_sys) {
      fb_we_sr := 0
    } otherwise {
      fb_we_sr := (drawing.asBits ## fb_we_sr(LAT_ADDR-1 downto 1))
    }
    fb_we  = fb_we_sr(0) && !clip
    //
    // read framebuffer for display output via linebuffer
    //
    // count lines for scaling via linebuffer
    val cnt_lb_line = Reg(UInt(log2Up(FB_SCALE) bits)) init(0)
    when(line0_sys) {
      cnt_lb_line := 0
    } otherwise {
      when(line_sys) {
        // Increment the counter or reset it to 0 if it reaches the FB_SCALE limit
        cnt_lb_line := Mux(cnt_lb_line === U(FB_SCALE - 1), U(0), cnt_lb_line + 1)
      }
    }
    // which screen lines need linebuffer?
    val lb_line = Reg(Bool()) init(False)

    when(line0_sys) {
      lb_line := True  // Enable when sy == 0
    } otherwise {
      when(frame_sys) {
        lb_line := False  // Disable at frame start
      }
    }
    // enable linebuffer input
    val lb_en_in = Bool()
    val cnt_lbx = Reg(UInt(log2Up(FB_WIDTH) bits)) init(0)  // Horizontal pixel counter
    lb_en_in := (lb_line && cnt_lb_line === 0 && cnt_lbx < FB_WIDTH)
    // calculate framebuffer read address for linebuffer
    when(line_sys) {
      // Reset horizontal counter at the start of a line
      cnt_lbx := 0
    } elsewhen(lb_en_in) {
      // Increment address when linebuffer is enabled
      fb_addr_read := fb_addr_read + 1
      cnt_lbx := cnt_lbx + 1
    }
    when(frame_sys) {
      // Reset address at the start of the frame
      fb_addr_read := 0
    }
    // enable linebuffer output
    val lb_en_out = Reg(Bool())  // Enable linebuffer output
    val LAT_LB = 3  // Output latency compensation: lb_en_out + 1, LB + 1, CLUT + 1
    when(sy >= FB_OFFY && sy < (FB_HEIGHT * FB_SCALE) + FB_OFFY &&
      sx >= FB_OFFX - LAT_LB && sx < (FB_WIDTH * FB_SCALE) + FB_OFFX - LAT_LB) {
      lb_en_out := True
    } otherwise {
      lb_en_out := False
    }
    // display linebuffer
    val lb_colr_out = Reg(UInt(FB_DATAW bits))  // Linebuffer output color
    // Instantiate the linebuffer (simple version)
    val linebuffer_instance = new linebuffer_simple()
    // Connect the ports of the linebuffer
    linebuffer_instance.clk_sys := clk_sys
    linebuffer_instance.clk_pix := io.clk_pix
    linebuffer_instance.line := line
    linebuffer_instance.line_sys := line_sys
    linebuffer_instance.en_in := lb_en_in
    linebuffer_instance.en_out := lb_en_out
    linebuffer_instance.scale := FB_SCALE
    linebuffer_instance.data_in := fb_colr_read.asBits.resize(FB_DATAW bits)
    lb_colr_out.asBits.resize(FB_DATAW bits) := linebuffer_instance.data_out
    // colour lookup table (CLUT)
    val fb_pix_colr = Reg(UInt(COLRW bits))init(0)  // Register to hold the pixel color
    val clut_instance = new clut_simple(
      COLRW = COLRW,
      CIDXW = CIDXW,
      F_PAL = PAL_FILE  // Path to the palette file
    )
    clut_instance.io.clk_write := io.clk_pix
    clut_instance.io.clk_read := io.clk_pix
    clut_instance.io.we := False    // Set to True if you want to enable write
    clut_instance.io.cidx_write := 0 // Write color index (0 in this case)
    clut_instance.io.cidx_read := lb_colr_out  // Read color index from linebuffer output
    clut_instance.io.colr_in := 0  // Optional input color data (can be set as needed)
    fb_pix_colr.asBits.resize(COLRW bits) := clut_instance.io.colr_out  // Output color from the CLUT
    // background colour (sy ignores 16:9 letterbox)
    val bg_colr = Reg(UInt(COLRW bits)) init(0)
    when(line) {
      when(sy === 0) {
        bg_colr := U"000000000000" // or U(0x000)
      } .elsewhen(sy === 60) {
        bg_colr := U"001000111001" // or U(0x239)
      } .elsewhen(sy === 140) {
        bg_colr := U"001001001010" // or U(0x24A)
      } .elsewhen(sy === 195) {
        bg_colr := U"001001101011" // or U(0x25B)
      } .elsewhen(sy === 230) {
        bg_colr := U"001001101100" // or U(0x26C)
      } .elsewhen(sy === 260) {
        bg_colr := U"001001111101" // or U(0x27D)
      } .elsewhen(sy === 285) {
        bg_colr := U"001010011110" // or U(0x29E)
      } .elsewhen(sy === 305) {
        bg_colr := U"001011111111" // or U(0x2BF)
      } .elsewhen(sy === 322) {
        bg_colr := U"011100000000" // or U(0x370)
      } .elsewhen(sy === 420) {
        bg_colr := U"000000000000" // or U(0x000)
      }
    }
    // paint screen
    val paint_area = Bool()
    val paint_r = Reg(UInt(CHANW bits)) init(0)
    val paint_g = Reg(UInt(CHANW bits)) init(0)
    val paint_b = Reg(UInt(CHANW bits)) init(0)
    when(de && (sy >= FB_OFFY && sy < (FB_HEIGHT * FB_SCALE) + FB_OFFY
      && sx >= FB_OFFX && sx < FB_WIDTH * FB_SCALE + FB_OFFX)) {
      paint_r := fb_pix_colr(11 downto 8) // Red channel (adjust based on your color format)
      paint_g := fb_pix_colr(7 downto 4)  // Green channel
      paint_b := fb_pix_colr(3 downto 0)  // Blue channel
    } otherwise {
      paint_r := 0
      paint_g := 0
      paint_b := 0
    }
    val show_bg = (de && (paint_r ## paint_g ## paint_b) === COLR_TRANS)
    when(io.clk_pix) {
      io.sdl_sx := sx
      io.sdl_sy := sy
      io.sdl_de := de
      io.sdl_frame := frame

      io.sdl_r := Mux(show_bg, bg_colr(11 downto 8).asSInt, paint_r.asSInt).resize(8)
      io.sdl_g := Mux(show_bg, bg_colr(7 downto 4).asSInt, paint_g.asSInt).resize(8)
      io.sdl_b := Mux(show_bg, bg_colr(3 downto 0).asSInt, paint_b.asSInt).resize(8)
    }otherwise {//default assignment
      io.sdl_sx := 0
      io.sdl_sy := 0
      io.sdl_de := False
      io.sdl_frame := False // Default assignment when the condition is not met
      io.sdl_r := 0
      io.sdl_g := 0
      io.sdl_b := 0
    }
  }
}
object top_castle_Verilog extends App {
  Config.spinal.generateVerilog(new top_castle())
}