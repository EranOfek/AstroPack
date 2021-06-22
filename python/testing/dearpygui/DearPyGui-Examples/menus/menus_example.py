from dearpygui.core import *
from dearpygui.simple import *


def theme_callback(sender, data):
    set_theme(sender)


with window("Main Window"):
    add_text("these is a basic menu, it does not use context managers from dearpygui.simple which are recomended.")
    add_menu_bar("MenuBar")

    add_menu("Themes")
    add_menu_item("Dark", callback=theme_callback)
    add_menu_item("Light", callback=theme_callback)
    add_menu_item("Classic", callback=theme_callback)
    add_menu_item("Dark 2", callback=theme_callback)
    add_menu_item("Grey", callback=theme_callback)
    add_menu_item("Dark Grey", callback=theme_callback)
    add_menu_item("Cherry", callback=theme_callback)
    add_menu_item("Purple", callback=theme_callback)
    add_menu_item("Gold", callback=theme_callback)
    add_menu_item("Red", callback=theme_callback)
    end()

    add_menu("Tools")
    add_menu_item("Show Logger", callback=show_logger)
    add_menu_item("Show About", callback=show_about)
    add_menu_item("Show Metrics", callback=show_metrics)
    add_menu_item("Show Documentation", callback=show_documentation)
    add_menu_item("Show Debug", callback=show_debug)
    end()

    end()

# secondary window with a menu
add_window("Secondary Window")
add_menu_bar("MenuBar2")
add_menu("Tools##2")
add_menu_item("Show Logger##2", callback=show_logger)
end()
end()
end()

# Third window with a menu - basically the same as the secondary window
with window("Third Window"):
    with menu_bar("MenuBar3"):
        with menu("Tools##3"):
            add_menu_item("Show Logger##3", callback=show_logger)
    add_text("This is a menu with context wrappers")

start_dearpygui(primary_window="Main Window")