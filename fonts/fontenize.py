#!/usr/bin/env python3

from os import path
import fontforge
import math
import psMat as mat
import sys
import unicodedata

HERE = path.dirname(__file__)

monosize = (600, 1000)

scpFile = path.join(HERE, "base-fonts", "SourceCodePro", "SourceCodePro-Regular.ttf")
dejavuFile = path.join(HERE, "base-fonts", "dejavu-fonts-ttf-2.37", "ttf", "DejaVuSansMono.ttf")
notoFile = path.join(HERE, "base-fonts", "NotoSansMono", "NotoSansMono-Regular.ttf")
bboldFile = path.join(HERE, "base-fonts", "bbold", "bbold10.pfa")
maiznerFile = path.join(HERE, "base-fonts", "Berthold Mainzer Fraktur", "Bertholdr Mainzer Fraktur.ttf")
felipaFile = path.join(HERE, "base-fonts", "felipa", "Felipa-Regular.ttf")
unifrackturFile = path.join(HERE, "base-fonts", "UnifrakturMaguntia.2017-03-19", "UnifrakturMaguntia.ttf")
amateurFile = path.join(HERE, "AmateurGlyphsForEexprs.ttf")


outFile = path.join(HERE, "EexprReferenceMono-Regular.sfd")

def main():
  scp = fontforge.open(scpFile)
  dejavu = fontforge.open(dejavuFile)
  noto = fontforge.open(notoFile)
  bbold = fontforge.open(bboldFile)
  felipa = fontforge.open(felipaFile)
  unifracktur = fontforge.open(unifrackturFile)
  amateur = fontforge.open(amateurFile)

  out = scp
  out.copyright = "Copyright (c) 2021 Zankoku Okuno, with Reserved Font Name 'Eexpr Reference'."
  out.familyname = "Eexpr Reference Mono"
  out.fontname = "EexprReferenceMono-Regular"
  out.fullname = "Eexpr Reference Mono"
  out.version = "0.001"
  out.sfntRevision = None

  dozenal(out, scp)
  script(out, felipa)
  fraktur(out, unifracktur)
  blackboard(out, bbold)
  subscripts(out, scp)
  hebrew(out, noto)
  customGlyphs(out, amateur)
  relations(out)
  setTheory(out, dejavu, amateur)
  operators(out, dejavu)
  nAry(out)
  arrows(out, dejavu)
  misc(out, noto)

  checkSizes(out)

  print(f"copyright: {out.copyright}")
  print(f"familyname: {out.familyname}")
  print(f"fondname: {out.fondname}")
  print(f"fontname: {out.fontname}")
  print(f"fullname: {out.fullname}")
  print(f"uniqueid: {out.uniqueid}")
  print(f"version: {out.version}")
  out.save(outFile)

def checkSizes(font):
  badWidth, badHeight = [], []
  for i in range(0, 0x110000):
    if i in font:
      if font[i].vwidth != monosize[1]:
        badHeight.append(chr(i))
      elif font[i].width != monosize[0]:
        badWidth.append(chr(i))
  numBadSize = len(badWidth) + len(badHeight)
  if numBadSize:
    print(f"{numBadSize} bad-sized chars")
    print(f"  {len(badHeight)} by height:")
    for c in badHeight:
      print(f"    U+{hex(ord(c))} {c}")
    print(f"  {len(badWidth)} by width:")
    for c in badWidth:
      print(f"    U+{hex(ord(c))} {c}")


# 0123456789↊↋
def dozenal(font, scp):
  scp.selection.select(("ranges",), ord("2"), ord("3"))
  scp.copy()
  font.selection.select(("ranges",), ord("↊"), ord("↋"))
  font.paste()
  if ord('↊') not in font: # rotate two to dek
    dek = font[ord('↊')]
    xform = aroundCentroid(dek, mat.rotate(math.pi))
    dek.transform(compose(
      xform,
      mat.scale(1, 0.96),
      mat.translate(0, -12),
    ), ('round',))
    dek.width, dek.vwidth = monosize
  if ord('↋') not in font: # rotate three to el
    el = font[ord('↋')]
    xform = aroundCentroid(el, mat.rotate(math.pi))
    el.transform(compose(
      xform,
      mat.scale(1, 0.96),
    ), ('round',))
    el.width, el.vwidth = monosize

def script(font, felipa):
  vratio = 681/754
  adjust = {
    # majuscule
    "𝒜": compose(mat.scale(0.9,1), mat.translate(50,0)),
    "𝒟": mat.translate(-20),
    "ℋ": mat.scale(0.87,1),
    "ℐ": mat.translate(30),
    "𝒥": mat.translate(50),
    "𝒦": mat.scale(0.93,1),
    "ℳ": mat.scale(0.72,1),
    "𝒩": mat.scale(0.9,1),
    "ℛ": mat.scale(0.95,1),
    "𝒮": mat.translate(50,0),
    "𝒰": mat.scale(0.9,1),
    "𝒱": mat.scale(0.9,1),
    "𝒲": mat.scale(0.67,1),
    "𝒳": mat.scale(0.85,1),
    "𝒴": compose(mat.scale(0.95,1), mat.translate(-15,0)),
    # miniscule
    "𝒻": mat.translate(100,0),
    "𝒾": mat.translate(100,0),
    "𝒿": mat.translate(100,0),
    "𝓂": compose(mat.translate(-90,0), mat.scale(0.98,1)),
    "𝓅": mat.translate(40,0),
    "𝓆": mat.translate(40,0),
    "𝓇": mat.translate(40,0),
    "𝓈": mat.translate(40,0),
    "𝓉": mat.translate(40,0),
    "𝓋": mat.translate(40,0),
    "𝓍": mat.translate(40,0),
    "𝓏": mat.translate(40,0),
  }
  # majuscule
  dst = "𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵"
  src = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  for i in range(0, len(dst)):
    felipa.selection.select(("singletons",), ord(src[i]))
    felipa.copy()
    font.selection.select(("singletons",), ord(dst[i]))
    font.paste()
    glyph = font[ord(dst[i])]
    xform = mat.scale(vratio)
    if dst[i] in adjust:
      xform = mat.compose(xform, adjust[dst[i]])
    glyph.transform(xform, ("round",))
    glyph.width, glyph.vwidth = monosize
  # miniscule
  dst = "𝒶𝒷𝒸𝒹ℯ𝒻ℊ𝒽𝒾𝒿𝓀𝓁𝓂𝓃ℴ𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏"
  src = "abcdefghijklmnopqrstuvwxyz"
  for i in range(0, len(dst)):
    felipa.selection.select(("singletons",), ord(src[i]))
    felipa.copy()
    font.selection.select(("singletons",), ord(dst[i]))
    font.paste()
    glyph = font[ord(dst[i])]
    xform = mat.scale(vratio)
    if dst[i] in adjust:
      xform = mat.compose(xform, adjust[dst[i]])
    xform = mat.compose(xform, mat.translate(60,0))
    glyph.transform(xform, ("round",))
    glyph.width, glyph.vwidth = monosize

def fraktur(font, unifracktur):
  unifracktur.selection.select(("singletons",), "i", "j", "u")
  unifracktur.unlinkReferences()
  dst = "𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜ℨ"
  src = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  vratio = 683/1471
  scale = [
    0.9, # A
    0.8, # B
    1.0, # C
    0.9, # D
    1.0, # E
    0.95, # F
    0.95, # G
    0.95, # H
    1.0, # I
    1.0, # J
    0.85, # K
    1.0, # L
    0.65, # M
    0.8, # N
    1.0, # O
    0.8, # P
    0.93, # Q
    0.82, # R
    0.95, # S
    0.95, # T
    1.0, # U
    0.85, # V
    0.65, # W
    1.0, # X
    0.83, # Y
    1.0, # Z
  ]

  adjust = {
    '𝔣': mat.translate(220, 0),
    '𝔦': mat.translate(-100, 0),
    '𝔧': mat.translate(-455, 0),
    '𝔩': mat.translate(1970, 0),
    '𝔰': mat.translate(-65, 0),
    '𝔵': mat.translate(200, 0),
  }
  # majuscule
  for i in range(0, len(dst)):
    unifracktur.selection.select(("singletons",), ord(src[i]))
    unifracktur.copy()
    font.selection.select(("singletons",), ord(dst[i]))
    font.paste()
    glyph = font[ord(dst[i])]
    xform = compose(
      mat.scale(vratio),
      mat.scale(scale[i], 1),
    )
    glyph.transform(xform, ("round",))
    xform = moveToCenter(glyph)
    if dst[i] in adjust:
      xform = mat.compose(xform, adjust[dst[i]])
    glyph.transform(xform, ("round",))
    glyph.width, glyph.vwidth = monosize
  # miniscule
  dst = "𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷"
  src = "abcdefghijklmnopqrstuvwxyz"
  for i in range(0, len(dst)):
    unifracktur.selection.select(("singletons",), ord(src[i]))
    unifracktur.copy()
    font.selection.select(("singletons",), ord(dst[i]))
    font.paste()
    glyph = font[ord(dst[i])]
    xform = mat.scale(vratio)
    glyph.transform(xform, ("round",))
    xform = moveToMonoNoSquash(glyph)
    if dst[i] in adjust:
      xform = mat.compose(xform, adjust[dst[i]])
    glyph.transform(xform, ("round",))
    # FIXME iju
    glyph.width, glyph.vwidth = monosize


def blackboard(font, bbold):
  dst = "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ" \
      + "𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫" \
      + "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" \
      + "ℼℽℾℿ⅀" \
      + "⨾∘"
  src = list("ABCDEFGHIJKLMNOPQRSTUVWXYZ") \
      + list("abcdefghijklmnopqrstuvwxyz") \
      + list("0123456789") \
      + [chr(x) for x in [25, 13, 0, 5, 6]] \
      + list(";=")
  adjust = {
    '𝕗': mat.translate(-100, 0),
    '𝕛': mat.translate(90, 0),
    '∘': mat.translate(0,95),
    '⨾': mat.translate(0,95),
  }
  for i in range(0, len(dst)):
    bbold.selection.select(("singletons",), ord(src[i]))
    bbold.copy()
    font.selection.select(("singletons",), ord(dst[i]))
    font.paste()
    glyph = font[ord(dst[i])]
    xform = moveToMonoNoSquash(glyph)
    if dst[i] in adjust:
      xform = mat.compose(xform, adjust[dst[i]])
    glyph.transform(xform, ('round',))
    glyph.width, glyph.vwidth = monosize

def subscripts(font, scp):
  dst = "ₐ"+"ₑ"+"ₕᵢⱼₖₗₘₙₒₚ"+"ᵣₛₜᵤᵥ"+"ₓ"
  src = "ᵃ"+"ᵉ"+"ʰⁱʲᵏˡᵐⁿᵒᵖ"+"ʳˢᵗᵘᵛ"+"ˣ"
  for i in range(0, len(dst)):
    scp.selection.select(("singletons",), ord(src[i]))
    scp.copy()
    font.selection.select(("singletons",), ord(dst[i]))
    font.paste()
    glyph = font[ord(dst[i])]
    xform = mat.translate(0, -576)
    glyph.transform(xform, ('round',))
    glyph.width, glyph.vwidth = monosize

def hebrew(font, noto):
  mvGlyph(font, noto, "ℵ")
  mvGlyph(font, noto, "ℶ")
  mvGlyph(font, noto, "ℷ")
  mvGlyph(font, noto, "ℸ")


def setTheory(font, dejavu, amateur):
  glyph = mvGlyph(font, dejavu, "⊂")
  xform = compose(mat.scale(1000/2048), mat.translate(0,20))
  glyph.transform(xform, ('round',))
  glyph.width, glyph.vwidth = monosize
  glyph = mvGlyph(font, font, "⊃", "⊂")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph.width, glyph.vwidth = monosize
  mvGlyph(font, amateur, "⊆")
  glyph = mvGlyph(font, font, "⊇", "⊆")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  mvGlyph(font, amateur, "∈")
  glyph = mvGlyph(font, font, "∋", "∈")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, "∪", "∩")
  xform = compose(
    aroundCentroid(glyph, mat.scale(1,-1)),
    mat.translate(0,-40),
  )
  glyph.transform(xform, ('round',))

def relations(font):
  glyph = mvGlyph(font, font, "⊐", "⊏")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, "⊒", "⊑")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, "≫", "≪")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, "⋙", "⋘")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, "≷", "≶")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, "⋛", "⋚")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))

def operators(font, dejavu):
  for c in "⋆⋄⊕⊖⊗⊘⊙⊚⊞⊟⊠⊡":
    glyph = mvGlyph(font, dejavu, c)
    xform = mat.scale(1000/2048)
    glyph.transform(xform, ('round',))
    glyph.width, glyph.vwidth = monosize
  for c in "≺≻≼≽⊀⊁⋠⋡≍≭≎≏":
    glyph = mvGlyph(font, dejavu, c)
    xform = mat.scale(1000/2048)
    glyph.transform(xform, ('round',))
    xform = aroundCentroid(glyph, mat.scale(0.8))
    if c in "≼≽⋠⋡":
      xform = mat.compose(xform, mat.translate(0,-70))
    glyph.transform(xform, ('round',))
    glyph.width, glyph.vwidth = monosize
  for c in "⊢":#⊨⊩⊫":
    glyph = mvGlyph(font, dejavu, c)
    xform = mat.scale(1000/2048)
    glyph.transform(xform, ('round',))
    glyph.width, glyph.vwidth = monosize




def arrows(font, dejavu):
  sideArrs = "←→↔↚↛↮" \
           + "⇐⇒⇔⇍⇏⇎" \
           + "↜↝↭⇜⇝" \
           + "↞↠↼⇀" \
           + "⇤⇥⟜⊸⧟" \
           + "↢↣↩↪↤↦" \
           + "⤆⤇" \
           + "⤙⤚⤛⤜"
  tallArrs = "↑↓↕⇑⇓⇕" \
           + "↟↡↾⇂" \
           + "⤒⤓⫯⫰↥↧"
  for c in sideArrs:
    if ord(c) in dejavu:
      dejavu.selection.select(("singletons",), ord(c))
      dejavu.copy()
      font.selection.select(("singletons",), ord(c))
      font.paste()
      glyph = font[ord(c)]
      xform = mat.scale(monosize[0]/glyph.width)
      glyph.transform(xform, ('round',))
      xFront, _, xBack, _ = glyph.layers[1].boundingBox()
      xform = aroundCentroid(glyph, mat.scale(glyph.width/(xBack-xFront + 20)))
      # xform = mat.translate(10 - xFront)
      glyph.transform(xform, ('round',))
      glyph.width, glyph.vwidth = monosize
  for c in tallArrs:
    if ord(c) in dejavu:
      dejavu.selection.select(("singletons",), ord(c))
      dejavu.copy()
      font.selection.select(("singletons",), ord(c))
      font.paste()
      glyph = font[ord(c)]
      _, yBot, _, yTop = glyph.layers[1].boundingBox()
      xform = mat.scale(monosize[0]/(yTop-yBot))
      glyph.transform(xform, ('round',))
      xform = moveToCenter(glyph)
      glyph.transform(xform, ('round',))
      glyph.width, glyph.vwidth = monosize
  glyph = mvGlyph(font, font, '⫯', '⊸')
  xform = aroundCentroid(glyph, mat.rotate(math.pi/2))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, '⫰', '⫯')
  xform = aroundCentroid(glyph, mat.scale(1, -1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, '⤓', '⤒')
  xform = aroundCentroid(glyph, mat.scale(1,-1))
  glyph.transform(xform, ('round',))
  glyph.width, glyph.vwidth = monosize
  glyph = mvGlyph(font, font, '⤆', '⤇')
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(font, font, '⤙', '⤚')
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph.width, glyph.vwidth = monosize
  glyph = mvGlyph(font, font, '⤛', '⤜')
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  glyph.width, glyph.vwidth = monosize

def nAry(out):
  glyph = mvGlyph(out, out, "∐","∏")
  xform = aroundCentroid(glyph, mat.scale(1,-1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(out, out, "⋃","⋂")
  xform = aroundCentroid(glyph, mat.scale(1,-1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(out, out, "⋁","⋀")
  xform = aroundCentroid(glyph, mat.scale(1,-1))
  glyph.transform(xform, ('round',))


def misc(out, noto):
  glyph = mvGlyph(out, noto, "℘")
  xform = compose(
    aroundCentroid(glyph, mat.scale(0.78,1)),
    moveToCenter(glyph),
  )
  glyph.transform(xform, ('round',))
  glyph.width, glyph.vwidth = monosize
  mvGlyph(out, noto, "ϰ")
  mvGlyph(out, out, "ϵ", "є")
  mvGlyph(out, out, "⋅", "∙")
  glyph = mvGlyph(out, out, "∓", "±")
  xform = aroundCentroid(glyph, mat.scale(1,-1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(out, out, "⅋", "&")
  xform = aroundCentroid(glyph, mat.rotate(math.pi))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(out, out, "⊥", "⊤")
  xform = aroundCentroid(glyph, mat.scale(1,-1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(out, out, "∇", "Δ")
  xform = aroundCentroid(glyph, mat.scale(1,-1))
  glyph.transform(xform, ('round',))
  glyph = mvGlyph(out, out, "≕", "≔")
  xform = aroundCentroid(glyph, mat.scale(-1,1))
  glyph.transform(xform, ('round',))
  mvGlyph(out, noto, "⋈")
  mvGlyph(out, noto, "⋉")
  mvGlyph(out, noto, "⋊")


def customGlyphs(font, custom):
  numCustom = 0
  numIgnored = 0
  force = "ϕ"
  for i in range(0, 0x110000):
    if i in custom:
      numCustom += 1
      if i not in font or chr(i) in force:
        custom.selection.select(("singletons",), i)
        custom.copy()
        font.selection.select(("singletons",), i)
        font.paste()
      else:
        numIgnored += 1
        print(f"ignoring U+{hex(i)} {chr(i)}")
  print(f"{numCustom - numIgnored}/{numCustom} custom characters transferred")


def aroundCentroid(glyph, xform):
  xLo, yLo, xHi, yHi = glyph.layers[1].boundingBox() # layers[1] is foreground
  xShift = (xHi - xLo) / 2 + xLo
  yShift = (yHi - yLo) / 2 + yLo
  into = mat.translate(-xShift, -yShift)
  outof = mat.translate(xShift, yShift)
  return mat.compose(mat.compose(into, xform), outof)

def moveToCenter(glyph):
  xFront, _, xBack, _ = glyph.layers[1].boundingBox()
  spaceBefore = xFront
  boundingWidth = xBack - xFront
  return mat.translate(monosize[0]/2 - spaceBefore - boundingWidth/2)

def moveToMonoNoSquash(glyph):
  # -------w--------
  #      ---ω---
  # --a--       -b--
  # |    (  .  )   |
  #       -x-
  # |  (  .  )  |
  # -----w'------
  # -a'-     -b'-
  #
  # x = (a + ω/2) = (a' - ω/2) = a - a'
  # a + ω + b = w
  # a' + ω + b' = w'
  # a/b = a'/b'
  #
  # a' = ab'/b
  # b' = w' - ω - a'
  # a' = a/b (w' - ω - a')
  #  a' + a/b(a') = a/b (w' - ω)
  #  a'(1 + a/b) = a/b(w' - ω)
  #  a' = a(w' - ω) ÷ b(1 + a/b)
  #  a' = a(w' - ω)/(b + a)
  # x = a - a(w' - ω)/(b + a)
  ### x = a(1 - (w' - ω)/(b - a)) ###
  xFront, _, xBack, _ = glyph.layers[1].boundingBox()
  spaceBefore, spaceAfter = xFront, glyph.width - xBack
  boundingWidth = xBack - xFront
  if spaceBefore + spaceAfter != 0:
    disp = spaceBefore * (1 - (monosize[0] - boundingWidth) / (spaceAfter + spaceBefore))
  else:
    disp = (spaceBefore + boundingWidth) / 2
  return mat.translate(-disp,0)


def compose(xform0, *xforms):
  for xform in xforms:
    xform0 = mat.compose(xform0, xform)
  return xform0

def mvGlyph(dstFont, srcFont, dst, src=None):
  if src is None: src = dst
  srcFont.selection.select(("singletons",), ord(src))
  srcFont.copy()
  dstFont.selection.select(("singletons",), ord(dst))
  dstFont.paste()
  return dstFont[ord(dst)]


if __name__ == "__main__":
  main()
