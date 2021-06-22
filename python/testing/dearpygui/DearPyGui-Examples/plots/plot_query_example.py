from dearpygui.core import *
from dearpygui.simple import *
from math import sin, cos, tan, factorial, exp, log, sqrt, log1p, log2, log10, acos, asin, atan, atan2, acosh, asinh
from math import atanh, e, pi, fmod, expm1, pow, hypot, degrees, radians,erf, erfc, gamma, lgamma, fabs


# callbacks
def query(sender, data):
    show_item("Plot Window")
    set_plot_xlimits("Plot2", data[0], data[1])
    set_plot_ylimits("Plot2", data[2], data[3])


def run_code(y, xmin, xmax, inc=100):
    y = str(y)
    code0 = "def tempfunc():\n"
    code8 = "    xmin =" + str(xmin) + "\n"
    code9 = "    xmax =" + str(xmax) + "\n"
    code1 = "    data = []\n"
    code2 = "    x = xmin\n"
    code3 = "    for i in range(0," + str(inc) + "+1):\n"
    code4 = "        y=" + y + "\n"
    code5 = "        data.append([x,y])\n"
    code6 = "        x = xmin + (1+i)*(xmax-xmin)/float(" + str(inc) + ")\n"
    code7 = "    return data"

    finalcode = code0 + code8 + code9 + code1 + code2 + code3 + code4 + code5 + code6 + code7

    try:
        exec(finalcode)
        data = eval("tempfunc()")
        return data
    except Exception as ex:
        return ex


def plot_callback(sender, data):
    clear_plot("Plot1")
    clear_plot("Plot2")

    ranges = get_value("Range")
    data1 = run_code(get_value("Y1"), ranges[0], ranges[1], get_value("Points"))

    x_data = []
    y_data = []
    for data in data1:
        x_data.append(data[0])
        y_data.append(data[1])

    add_line_series("Plot1", "Y1", x_data, y_data, weight=2, color=get_value("Color"))
    add_scatter_series("Plot1", "Y1 Scatter", x_data, y_data, weight=2, outline=get_value("Color"), fill=get_value("Fill"))
    add_line_series("Plot2", "Y1", x_data, y_data, weight=2, color=get_value("Color"))
    add_scatter_series("Plot2", "Y1 Scatter", x_data, y_data, weight=2, outline=get_value("Color"), fill=get_value("Fill"))

with window("Main Window"):
    with group("Left Panel", width=200):
        add_button("Plot data", callback=plot_callback)
        add_input_text("Y1", default_value="40*sin(x)")
        add_input_int("Points", default_value=300, callback=plot_callback)
        add_input_float2("Range", default_value=[-10, 10], callback=plot_callback)
        add_spacing(count=5)
        add_color_picker4("Color", default_value=[255, 0, 0, 255], callback=plot_callback)
        add_color_picker4("Fill", default_value=[255, 0, 0, 100], callback=plot_callback)

    add_same_line()
    add_plot("Plot1", height=-1, query_callback=query)

with window("Plot Window", width=500, height=500, show=False):
        add_plot("Plot2", height=-1)

start_dearpygui(primary_window="Main Window")
