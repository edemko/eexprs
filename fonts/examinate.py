#!/usr/bin/env python3

from os import path
import sys
import fontforge
import unicodedata


def main():
  allChars = list(eexprChars())
  # allChars = list(eexprCharsFull())

  numAllChars = len(allChars)
  print(f"eexpr symbol chars: {numAllChars}")

  missing = allChars
  for fontFile in sys.argv[1:]:
    font = fontforge.open(fontFile)
    missing = list(missingChars(missing, font))
    numMissing = len(missing)
    print(f"ABOUT {font.fontname}:")
    print(f"  sizes: {fontSizes(font)}")
    permille = int(10000*(numAllChars - numMissing) / numAllChars)
    percentInt = permille // 100
    percentFrac = permille % 100
    print(f"  missing: {numMissing} ({percentInt}.{percentFrac}% coverage)")

  for i in missing:
    print(f"U+{hex(i)} {chr(i)}")


def missingChars(rng, font):
  for i in rng:
    if i not in font:
      yield i

def fontSizes(font):
  acc = set()
  for i in range(0, 0x110000):
    if i in font:
      acc.add((font[i].width, font[i].vwidth))
  return acc

def eexprChars():
  # alphanum
  yield from charRange('A', 'Z')
  yield from charRange('a', 'z')
  yield from charRange('0', '9')
  yield from charString('↊↋')
  # asciisymb
  yield from charString('!$%&\'*+-/<=>?@^_|~')
  # letterlike
  yield from charString('ℓ№℘ℏ‽†‡')
  # greekAlpha
  yield from skip(charRange('Α','Ω'), [0x03A2])
  yield from charRange('α', 'ω')
  yield from charString('ϐϴϑϒϕϖϰϱϵ')
  # mathHebrew
  yield from charRange('ℵ', 'ℸ')
  # super- and subscripts
  yield from charString('ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖ'+'ʳˢᵗᵘᵛʷˣʸᶻ')
  yield from charString('⁰¹²³⁴⁵⁶⁷⁸⁹')
  yield from charString('ₐ'+'ₑ'+'ₕᵢⱼₖₗₘₙₒₚ'+'ᵣₛₜᵤᵥ'+'ₓ')
  yield from charString('₀₁₂₃₄₅₆₇₈₉')

  
  
  
  # mathAlpha
  yield from charString("𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵"
                       +"𝒶𝒷𝒸𝒹ℯ𝒻ℊ𝒽𝒾𝒿𝓀𝓁𝓂𝓃ℴ𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏")
  yield from charString("𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜ℨ"
                       +"𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷")
  # blackboard
  yield from nonReserved(charRange('𝔸', '𝕫'))
  yield from charString('ℂℍℕℙℚℝℤ')
  yield from charRange('𝟘', '𝟡')
  yield from charString('ℼℽℾℿ⅀')

  # math symbols
  yield from charString('∅∞∩∪⨿×∈∋∉∌⊂⊃⊆⊇⊄⊅⊈⊉') # setSymb
  yield from charString('⊤⊥∀∃∄¬∧∨⊻⊼⊽⅋□◊') # logicSymb
  yield from charString('±∓⋅÷√‰‱') # arithSymb
  yield from charString('∂∫∇∆') # calculusSymb
  yield from charString('∘⨾⧺⋆∗⋄⊕⊖⊗⊘⊛⊙⊚⊞⊟⊠⧄⧆⊡⧇⋈⋉⋊') # mathOps
  yield from charString('≠≡≢≃≄≅≇≁≈≉≟⩵⩶≝≜⩴≔≕≍≭≎≏') # equivRels
  yield from charString('≤≥≮≯≰≱≺≻≼≽⊀⊁⋠⋡⊏⊐⊑⊒⋢⋣≶≷≸≹⋚⋛≲≳≴≵≪≫⋘⋙') # compareRels
  yield from charString('⊢⊨⊩⊫') # mathTurnstiles
  yield from charString('⋃⋂∐⨉⋁⋀∑∏⨊⨁⨂⨀') # nArySymb
  # arrows
  yield from charString('←↑→↓↔↕↚↛↮⇐⇑⇒⇓⇔⇕⇍⇏⇎') # plain
  yield from charString('↜↝↭⇜⇝') # alternate bodies
  yield from charString('↞↟↠↡↼↾⇀⇂⇤⤒⇥⤓⟜⫯⊸⫰') # alternate heads
  yield from charString('↢↣↩↪↤↥↦↧⤆⤇') # alternate tails
  yield from charString('⤙⤚⤛⤜') # only tails

def eexprCharsFull():
  yield from eexprChars()
  yield from eexprUnlikely()

def eexprUnlikely():
  # archaic, numeral, and variant greek
  yield from charRange('Ͱ', 'ͳ')
  yield from charRange('ϗ', 'ϡ')
  yield from charRange('Ϻ', 'ϻ')
  yield from charString('϶ϼ')
  # unlikely math
  yield from charString('∴∵∷') # extra dots
  yield from charString('⧻∔∸≀') # random operators
  yield from charString('⦼⨸⊜⊝⨹⨺⨻') # circled operators
  yield from charString('≣≋∝') # equivalence relations
  yield from charString('⊊⊋⋤⋥⪇⪈⋦⋧') # proper relations
  yield from charString('⊲⊳⊴⊵⋪⋫⋬⋭') # closed triangle relations
  yield from charString('⋞⋟⪮≬') # other relations
  yield from charString('⬠⬡○⬭⬯◇☆⯎⯏⋕⩩') # math shapes
  # unlikely arrows
  yield from charString('⇚⇛⤊⤋⟰⟱⭅⭆')
  yield from charString('⤸⤹⤺⤻⥊⥋⥌⥍⥎⥏⥐⥑↽↿⇁⇃')
  yield from charString('⧟')
  yield from charString('↫↬⥼⥽⥾⥿⤅⬶')
  yield from charString('↰↱↲↳⬎⬏⬐⬑⤴⤵⤶⤷↶↷⤾⤿⮌⮍⮎⮏↺↻⟲⟳⥀⥁')
  yield from charString('⇄⇅⇆⇇⇈⇉⇊⇵⥢⥣⥤⥥⥮⥯⇋⇌⥦⥧⥨⥩⥪⥫⥬⥭⇶⬱')

def charString(s):
  for i in s: yield ord(i)
def charRange(start, end):
  for i in range(ord(start), ord(end) + 1):
    yield i
def skip(rng, excl):
  for i in rng:
    if i not in excl:
      yield i
def nonReserved(rng):
  for i in rng:
    if unicodedata.category(chr(i)) != 'Cn':
      yield i

if __name__ == "__main__":
  main()
