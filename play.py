from re import re

import mistletoe as md
from mistletoe.span_token import SpanToken


class GithubWiki(SpanToken):
    # TODO add wikilinks
    # TODO add wiki images
    pattern = re.compile(r"\[\[ *(.+?) *\| *(.+?) *\]\]")
    
    def __init__(self, match_obj):
      self.target = match_obj.group(2)
      
class WikiComment(SpanToken):
  # TODO add comment
    pattern = re.compile(r"\[\[ *(.+?) *\| *(.+?) *\]\]")
    
    def __init__(self, match_obj):
      self.target = match_obj.group(2)

        
        
with open('lg.md', 'r', encoding='utf-8') as md_file:
    doc = md.Document(md_file)
