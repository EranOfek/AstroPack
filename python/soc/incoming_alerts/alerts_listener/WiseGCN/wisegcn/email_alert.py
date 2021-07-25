import smtplib
from os.path import basename
from email.mime.application import MIMEApplication
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.utils import COMMASPACE, formatdate
from configparser import ConfigParser
import logging

config = ConfigParser(inline_comment_prefixes=';')
config.read("config.ini")


def send_mail(subject, text, html="",
              send_from=config.get('EMAIL', 'FROM'),
              send_to=[e.strip() for e in config.get('EMAIL', 'TO').split(',')],
              cc_to=[e.strip() for e in config.get('EMAIL', 'CC').split(',')] if config.has_option('EMAIL', 'CC') else '',
              bcc_to=[e.strip() for e in config.get('EMAIL', 'BCC').split(',')] if config.has_option('EMAIL', 'BCC') else '',
              files=None,
              server=config.get('EMAIL', 'SERVER') if config.has_option('EMAIL', 'SERVER') else 'localhost',
              log=None):
    # based on: https://stackoverflow.com/questions/3362600/how-to-send-email-attachments

    assert isinstance(send_to, list)

    if log is None:
        log = logging.getLogger(__name__)

    msg = MIMEMultipart()
    msg['From'] = send_from
    msg['To'] = COMMASPACE.join(send_to)
    msg['CC'] = COMMASPACE.join(cc_to)
    msg['BCC'] = COMMASPACE.join(bcc_to)
    msg['Date'] = formatdate(localtime=True)
    msg['Subject'] = subject

    msg.attach(MIMEText(text, 'plain'))
    if not (not html):
        msg.attach(MIMEText(html, 'html'))

    for f in files or []:
        with open(f, "rb") as fil:
            part = MIMEApplication(
                fil.read(),
                Name=basename(f)
            )
        # After the file is closed
        part['Content-Disposition'] = 'attachment; filename="%s"' % basename(f)
        msg.attach(part)

    try:
        smtp = smtplib.SMTP(server)
        smtp.sendmail(send_from, send_to+cc_to+bcc_to, msg.as_string())
        smtp.close()
        log.debug("Email sent.")
    except Exception as e:
        code, msg = e.args
        log.error("Failed to send email! Error {}: {}".format(code, msg))

    return


def format_html(text, img=None, img_width=300):
    if img is not None:
        html = f"""\
        <html>
          <head></head>
          <body>
            <p>
                {text}
            </p>
            <p>
                <img src="{img}" width="{img_width}" border="0">
            </p>
          </body>
        </html>
        """
    else:
        html = f"""\
        <html>
          <head></head>
          <body>
            <p>
                {text}
            </p>
          </body>
        </html>
        """

    return html


def format_alert(params, area=None):
    from astropy.time import Time
    import numpy as np
    from wisegcn.utils import get_sky_area

    # healpix map image path
    image_url = params["skymap_fits"][0:params["skymap_fits"].find("fits.gz")]

    t = Time.now()
    t.format = "iso"

    html = f"""\
    <html>
      <head></head>
      <body>
        <p>
            <b>{params["AlertType"]} Alert</b><br>
            <br>
            <b>GraceID:</b> <a href="{params["EventPage"]}">{params["GraceID"]}</a><br>
            <b>Event UT:</b> {params["isotime"]}<br>
            <b>Alert UT:</b> {params["date_ivorn"]}<br>
            <b>Ingestion UT:</b> {t.value}<br>
            <b>FAR [yr<sup>-1</sup>]:</b> 1/{np.round(1/(float(params["FAR"])*60*60*24*365),2)}<br>
            <b>Detectors:</b> {params["Instruments"]}<br>
    """

    if area is not None:
        html = html + f"""\
            <b>50% Probability Area [deg<sup>2</sup>]:</b> {np.round(area[0], 2)}<br>
            <b>90% Probability Area [deg<sup>2</sup>]:</b> {np.round(area[1], 2)}<br>
        """

    html = html + f"""\
            <b>Nature [BNS / NSBH / MassGap / BBH / Terrestrial]:</b> {np.round(float(params["BNS"])*100, 1)}% / 
                {np.round(float(params["NSBH"])*100, 1)}% / 
                {np.round(float(params["MassGap"])*100, 1)}% / 
                {np.round(float(params["BBH"])*100, 1)}% / 
                {np.round(float(params["Terrestrial"])*100, 1)}%<br>
            <b>Probability of NS Component:</b> {np.round(float(params["HasNS"])*100, 1)}%<br>
            <b>Probability of Remnant Emission:</b> {np.round(float(params["HasRemnant"])*100, 1)}%<br>
        </p>
        <p>
            <img src="{image_url+"png"}" width="500" border="0"> 
            <img src="{image_url+"volume.png"}" width="300" border="0">
        </p>
      </body>
    </html>
    """

    return html
