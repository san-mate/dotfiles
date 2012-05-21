require 'cairo'

-------------------------------------------------------------------------------
--                                                              Draw Background
-- Change these settings to affect your background.
corner_r  = 0          -- radius of the rounded corners, 0 for no rounded
bg_colour = 0x000000   -- background colour
bg_alpha  = 0.4        -- background transparency (alpha)

-------------------------------------------------------------------------------
--                                                                 rgb_to_r_g_b
-- converts color in hexa to decimal
--
function rgb_to_r_g_b(colour, alpha)
    return ((colour / 0x10000) % 0x100) / 255., ((colour / 0x100) % 0x100) / 255., (colour % 0x100) / 255., alpha
end


-------------------------------------------------------------------------------
--                                                                    conky_pad
-- pad values with spaces to keep rasonable size
--
function conky_pad(val)
    return string.format('%s', conky_parse(val))
end


-------------------------------------------------------------------------------
--                                                        conky_draw_background
-- draw semi-transparent background, modify with the settings on the top
--
function conky_draw_background()
    if conky_window==nil then
        return
    end

    local w = conky_window.width
    local h = conky_window.height
    local cs = cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, w, h)
    cr = cairo_create(cs)
    
    cairo_move_to(cr,corner_r,0)
    cairo_line_to(cr,w-corner_r,0)
    cairo_curve_to(cr,w,0,w,0,w,corner_r)
    cairo_line_to(cr,w,h-corner_r)
    cairo_curve_to(cr,w,h,w,h,w-corner_r,h)
    cairo_line_to(cr,corner_r,h)
    cairo_curve_to(cr,0,h,0,h,0,h-corner_r)
    cairo_line_to(cr,0,corner_r)
    cairo_curve_to(cr,0,0,0,0,corner_r,0)
    cairo_close_path(cr)
    
    cairo_set_source_rgba(cr,rgb_to_r_g_b(bg_colour,bg_alpha))
    cairo_fill(cr)

    cairo_surface_destroy(cs)
    cairo_destroy(cr)
end
