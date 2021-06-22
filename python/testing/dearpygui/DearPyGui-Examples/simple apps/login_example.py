from dearpygui.core import *
from dearpygui.simple import *


def recursively_show(container):
    for item in get_item_children(container):
        if get_item_children(item):
            show_item(item)
            recursively_show(item)
        else:
            show_item(item)


def try_login(sender, data):
    print(sender)
    print(data)
    if get_value('Password') == 'password':
        delete_item('Login Window')
        recursively_show('Main Window')
    else:
        show_item('Incorrect Password.')


def position_login_window(sender, data):
    if does_item_exist('Login Window'):
        main_width = get_item_width('Main Window')
        main_height = get_item_height('Main Window')
        login_width = get_item_width('Login Window')
        login_height = get_item_height('Login Window')
        set_window_pos('Login Window', int((main_width/2 - login_width/2)), int((main_height/2 - login_height/2)))
    else:
        # this replaces the callback with None so we dont waste resources running the login window position callback
        set_render_callback(None)


with window('Login Window', no_title_bar=True, autosize=True, no_resize=True):
    add_input_text('Password', hint='hover me for password', password=True, on_enter=True, callback=try_login, tip='the password is "password"')
    add_button('Login', callback=try_login)
    add_text('Incorrect Password.', color=[255, 0, 0], parent='Login Window')
    hide_item('Incorrect Password.')
    set_render_callback(position_login_window)
with window("Main Window"):
    with menu_bar('menu bar'):
        with menu('menu 1'):
            add_menu_item('menu item')
        with menu('menu 2'):
            add_menu_item('menu item 2')

    add_text('Congrats!, you may now use the app')
    add_button('Button 1')
    hide_item('Main Window', children_only=True)
start_dearpygui(primary_window="Main Window")
