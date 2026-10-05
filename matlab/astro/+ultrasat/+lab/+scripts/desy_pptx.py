#!/usr/bin/env python3
"""A very small PowerPoint writer: titles, bullets, tables and pictures.

A .pptx is a zip of XML parts, so a deck of plain slides needs no library -- and
this machine has none. Only what these slides use is implemented: a 16:9 deck
with one blank layout, and per slide a title, an optional context strip, bullet
text, a simple table and any number of pictures placed by rectangle with their
aspect ratio preserved.
"""
import os, zipfile
from xml.sax.saxutils import escape

EMU = 914400                      # per inch
W, H = int(13.333*EMU), int(7.5*EMU)

def _pic_size(path):
    """(w, h) in pixels, from the PNG header"""
    with open(path, 'rb') as fh:
        d = fh.read(26)
    if d[:8] != b'\x89PNG\r\n\x1a\n':
        return (1600, 900)
    return (int.from_bytes(d[16:20], 'big'), int.from_bytes(d[20:24], 'big'))

def _tx(txt, sz, bold=False, color='1A1A1A', align='l', italic=False):
    runs = ''
    for i, line in enumerate(str(txt).split('\n')):
        br = '<a:br/>' if i else ''
        runs += (f'{br}<a:r><a:rPr lang="en-US" sz="{sz}" b="{1 if bold else 0}" '
                 f'i="{1 if italic else 0}" dirty="0"><a:solidFill>'
                 f'<a:srgbClr val="{color}"/></a:solidFill>'
                 f'<a:latin typeface="Calibri"/></a:rPr><a:t>{escape(line)}</a:t></a:r>')
    return f'<a:p><a:pPr algn="{align}"/>{runs}</a:p>'

class Deck:
    def __init__(self):
        self.slides = []          # each: (shapes_xml, [(rid, path)])
        self.media = []

    # ---------------------------------------------------------------- slides
    def add(self):
        self.slides.append({'shapes': [], 'pics': []})
        return self

    def _sid(self):
        return len(self.slides[-1]['shapes']) + len(self.slides[-1]['pics']) + 2

    def text(self, x, y, w, h, txt, sz=1800, bold=False, color='1A1A1A',
             align='l', anchor='t', italic=False):
        body = txt if isinstance(txt, list) else [txt]
        paras = ''.join(_tx(t, sz, bold, color, align, italic) for t in body)
        self.slides[-1]['shapes'].append(
            f'<p:sp><p:nvSpPr><p:cNvPr id="{self._sid()}" name="t{self._sid()}"/>'
            f'<p:cNvSpPr txBox="1"/><p:nvPr/></p:nvSpPr><p:spPr>'
            f'<a:xfrm><a:off x="{int(x)}" y="{int(y)}"/><a:ext cx="{int(w)}" cy="{int(h)}"/></a:xfrm>'
            f'<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr>'
            f'<p:txBody><a:bodyPr wrap="square" anchor="{anchor}"><a:normAutofit/></a:bodyPr>'
            f'<a:lstStyle/>{paras}</p:txBody></p:sp>')
        return self

    def rect(self, x, y, w, h, fill='F2F4F7'):
        self.slides[-1]['shapes'].append(
            f'<p:sp><p:nvSpPr><p:cNvPr id="{self._sid()}" name="r{self._sid()}"/>'
            f'<p:cNvSpPr/><p:nvPr/></p:nvSpPr><p:spPr>'
            f'<a:xfrm><a:off x="{int(x)}" y="{int(y)}"/><a:ext cx="{int(w)}" cy="{int(h)}"/></a:xfrm>'
            f'<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>'
            f'<a:solidFill><a:srgbClr val="{fill}"/></a:solidFill>'
            f'<a:ln><a:noFill/></a:ln></p:spPr>'
            f'<p:txBody><a:bodyPr/><a:lstStyle/><a:p/></p:txBody></p:sp>')
        return self

    def picture(self, path, x, y, w, h):
        """place inside the rectangle, keeping the aspect ratio, centred"""
        if not os.path.isfile(path):
            return self.text(x, y+h/2, w, EMU//2, f'[missing: {os.path.basename(path)}]',
                             sz=1200, color='C44E52', align='ctr')
        pw, ph = _pic_size(path)
        s = min(w/pw, h/ph)
        iw, ih = int(pw*s), int(ph*s)
        ox, oy = int(x + (w-iw)/2), int(y + (h-ih)/2)
        if path not in self.media:
            self.media.append(path)
        mi = self.media.index(path) + 1
        rid = f'rId{len(self.slides[-1]["pics"]) + 100}'
        self.slides[-1]['pics'].append((rid, mi))
        self.slides[-1]['shapes'].append(
            f'<p:pic><p:nvPicPr><p:cNvPr id="{self._sid()}" name="p{self._sid()}"/>'
            f'<p:cNvPicPr/><p:nvPr/></p:nvPicPr>'
            f'<p:blipFill><a:blip r:embed="{rid}"/><a:stretch><a:fillRect/></a:stretch></p:blipFill>'
            f'<p:spPr><a:xfrm><a:off x="{ox}" y="{oy}"/><a:ext cx="{iw}" cy="{ih}"/></a:xfrm>'
            f'<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr></p:pic>')
        return self

    def table(self, x, y, w, rows, colw=None, sz=1100, header=True):
        n = len(rows[0])
        colw = colw or [w//n]*n
        gr = ''.join(f'<a:gridCol w="{int(c)}"/>' for c in colw)
        trs = ''
        for ir, row in enumerate(rows):
            hdr = header and ir == 0
            fill = 'E8ECF2' if hdr else ('FFFFFF' if ir % 2 else 'FAFBFC')
            tcs = ''
            for cell in row:
                tcs += (f'<a:tc><a:txBody><a:bodyPr/><a:lstStyle/>'
                        f'{_tx(cell, sz, hdr)}</a:txBody>'
                        f'<a:tcPr marL="45720" marR="45720" marT="27432" marB="27432" anchor="ctr">'
                        f'<a:solidFill><a:srgbClr val="{fill}"/></a:solidFill></a:tcPr></a:tc>')
            trs += f'<a:tr h="{int(0.32*EMU)}">{tcs}</a:tr>'
        self.slides[-1]['shapes'].append(
            f'<p:graphicFrame><p:nvGraphicFramePr>'
            f'<p:cNvPr id="{self._sid()}" name="tbl{self._sid()}"/><p:cNvGraphicFramePr/><p:nvPr/>'
            f'</p:nvGraphicFramePr><p:xfrm><a:off x="{int(x)}" y="{int(y)}"/>'
            f'<a:ext cx="{int(w)}" cy="{int(0.32*EMU*len(rows))}"/></p:xfrm>'
            f'<a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/table">'
            f'<a:tbl><a:tblPr firstRow="1" bandRow="1"/><a:tblGrid>{gr}</a:tblGrid>{trs}</a:tbl>'
            f'</a:graphicData></a:graphic></p:graphicFrame>')
        return self

    # ---------------------------------------------------------------- write
    def save(self, path):
        ns = ('xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" '
              'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" '
              'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"')
        n = len(self.slides)
        with zipfile.ZipFile(path, 'w', zipfile.ZIP_DEFLATED) as z:
            over = ''.join(
                f'<Override PartName="/ppt/slides/slide{i+1}.xml" ContentType="application/vnd.'
                f'openxmlformats-officedocument.presentationml.slide+xml"/>' for i in range(n))
            z.writestr('[Content_Types].xml',
                '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
                '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
                '<Default Extension="xml" ContentType="application/xml"/>'
                '<Default Extension="png" ContentType="image/png"/>'
                '<Override PartName="/ppt/presentation.xml" ContentType="application/vnd.'
                'openxmlformats-officedocument.presentationml.presentation.main+xml"/>'
                '<Override PartName="/ppt/slideMasters/slideMaster1.xml" ContentType="application/vnd.'
                'openxmlformats-officedocument.presentationml.slideMaster+xml"/>'
                '<Override PartName="/ppt/slideLayouts/slideLayout1.xml" ContentType="application/vnd.'
                'openxmlformats-officedocument.presentationml.slideLayout+xml"/>'
                '<Override PartName="/ppt/theme/theme1.xml" ContentType="application/vnd.'
                'openxmlformats-officedocument.theme+xml"/>' + over + '</Types>')
            z.writestr('_rels/.rels',
                '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/'
                'relationships/officeDocument" Target="ppt/presentation.xml"/></Relationships>')
            sldids = ''.join(f'<p:sldId id="{256+i}" r:id="rId{i+2}"/>' for i in range(n))
            z.writestr('ppt/presentation.xml',
                f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?><p:presentation {ns}>'
                f'<p:sldMasterIdLst><p:sldMasterId id="2147483648" r:id="rId1"/></p:sldMasterIdLst>'
                f'<p:sldIdLst>{sldids}</p:sldIdLst>'
                f'<p:sldSz cx="{W}" cy="{H}"/><p:notesSz cx="{H}" cy="{W}"/></p:presentation>')
            rels = ('<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/'
                    '2006/relationships/slideMaster" Target="slideMasters/slideMaster1.xml"/>')
            rels += ''.join(
                f'<Relationship Id="rId{i+2}" Type="http://schemas.openxmlformats.org/officeDocument/'
                f'2006/relationships/slide" Target="slides/slide{i+1}.xml"/>' for i in range(n))
            rels += (f'<Relationship Id="rId{n+2}" Type="http://schemas.openxmlformats.org/officeDocument/'
                     f'2006/relationships/theme" Target="theme/theme1.xml"/>')
            z.writestr('ppt/_rels/presentation.xml.rels',
                '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                + rels + '</Relationships>')
            z.writestr('ppt/slideMasters/slideMaster1.xml',
                f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?><p:sldMaster {ns}>'
                f'<p:cSld><p:spTree><p:nvGrpSpPr><p:cNvPr id="1" name=""/><p:cNvGrpSpPr/><p:nvPr/>'
                f'</p:nvGrpSpPr><p:grpSpPr/></p:spTree></p:cSld>'
                f'<p:clrMap bg1="lt1" tx1="dk1" bg2="lt2" tx2="dk2" accent1="accent1" accent2="accent2" '
                f'accent3="accent3" accent4="accent4" accent5="accent5" accent6="accent6" hlink="hlink" '
                f'folHlink="folHlink"/><p:sldLayoutIdLst><p:sldLayoutId id="2147483649" r:id="rId1"/>'
                f'</p:sldLayoutIdLst></p:sldMaster>')
            z.writestr('ppt/slideMasters/_rels/slideMaster1.xml.rels',
                '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/'
                'relationships/slideLayout" Target="../slideLayouts/slideLayout1.xml"/>'
                '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/'
                'relationships/theme" Target="../theme/theme1.xml"/></Relationships>')
            z.writestr('ppt/slideLayouts/slideLayout1.xml',
                f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                f'<p:sldLayout {ns} type="blank" preserve="1"><p:cSld name="Blank"><p:spTree>'
                f'<p:nvGrpSpPr><p:cNvPr id="1" name=""/><p:cNvGrpSpPr/><p:nvPr/></p:nvGrpSpPr>'
                f'<p:grpSpPr/></p:spTree></p:cSld><p:clrMapOvr><a:overrideClrMapping bg1="lt1" '
                f'tx1="dk1" bg2="lt2" tx2="dk2" accent1="accent1" accent2="accent2" accent3="accent3" '
                f'accent4="accent4" accent5="accent5" accent6="accent6" hlink="hlink" '
                f'folHlink="folHlink"/></p:clrMapOvr></p:sldLayout>')
            z.writestr('ppt/slideLayouts/_rels/slideLayout1.xml.rels',
                '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/'
                'relationships/slideMaster" Target="../slideMasters/slideMaster1.xml"/></Relationships>')
            scheme = ''.join(f'<a:{k}><a:srgbClr val="{v}"/></a:{k}>' for k, v in (
                ('dk1','000000'), ('lt1','FFFFFF'), ('dk2','1F3864'), ('lt2','EEECE1'),
                ('accent1','4C72B0'), ('accent2','C44E52'), ('accent3','55A868'),
                ('accent4','8172B2'), ('accent5','DD8452'), ('accent6','937860'),
                ('hlink','0563C1'), ('folHlink','954F72')))
            z.writestr('ppt/theme/theme1.xml',
                f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                f'<a:theme xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" name="WIS">'
                f'<a:themeElements><a:clrScheme name="WIS">{scheme}</a:clrScheme>'
                f'<a:fontScheme name="WIS"><a:majorFont><a:latin typeface="Calibri"/><a:ea typeface=""/>'
                f'<a:cs typeface=""/></a:majorFont><a:minorFont><a:latin typeface="Calibri"/>'
                f'<a:ea typeface=""/><a:cs typeface=""/></a:minorFont></a:fontScheme>'
                f'<a:fmtScheme name="WIS"><a:fillStyleLst><a:solidFill><a:schemeClr val="phClr"/>'
                f'</a:solidFill><a:solidFill><a:schemeClr val="phClr"/></a:solidFill><a:solidFill>'
                f'<a:schemeClr val="phClr"/></a:solidFill></a:fillStyleLst><a:lnStyleLst>'
                f'<a:ln w="9525"><a:solidFill><a:schemeClr val="phClr"/></a:solidFill></a:ln>'
                f'<a:ln w="9525"><a:solidFill><a:schemeClr val="phClr"/></a:solidFill></a:ln>'
                f'<a:ln w="9525"><a:solidFill><a:schemeClr val="phClr"/></a:solidFill></a:ln>'
                f'</a:lnStyleLst><a:effectStyleLst><a:effectStyle><a:effectLst/></a:effectStyle>'
                f'<a:effectStyle><a:effectLst/></a:effectStyle><a:effectStyle><a:effectLst/>'
                f'</a:effectStyle></a:effectStyleLst><a:bgFillStyleLst><a:solidFill>'
                f'<a:schemeClr val="phClr"/></a:solidFill><a:solidFill><a:schemeClr val="phClr"/>'
                f'</a:solidFill><a:solidFill><a:schemeClr val="phClr"/></a:solidFill>'
                f'</a:bgFillStyleLst></a:fmtScheme></a:themeElements></a:theme>')
            for i, path in enumerate(self.media):
                z.write(path, f'ppt/media/image{i+1}.png')
            for i, sl in enumerate(self.slides):
                z.writestr(f'ppt/slides/slide{i+1}.xml',
                    f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?><p:sld {ns}><p:cSld>'
                    f'<p:spTree><p:nvGrpSpPr><p:cNvPr id="1" name=""/><p:cNvGrpSpPr/><p:nvPr/>'
                    f'</p:nvGrpSpPr><p:grpSpPr/>' + ''.join(sl['shapes']) +
                    f'</p:spTree></p:cSld><p:clrMapOvr><a:masterClrMapping/></p:clrMapOvr></p:sld>')
                prs = ''.join(
                    f'<Relationship Id="{rid}" Type="http://schemas.openxmlformats.org/officeDocument/'
                    f'2006/relationships/image" Target="../media/image{mi}.png"/>'
                    for rid, mi in sl['pics'])
                z.writestr(f'ppt/slides/_rels/slide{i+1}.xml.rels',
                    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/'
                    '2006/relationships/slideLayout" Target="../slideLayouts/slideLayout1.xml"/>'
                    + prs + '</Relationships>')
        return path
