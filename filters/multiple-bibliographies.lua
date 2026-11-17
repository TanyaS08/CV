--[[
Copyright © 2018-2022 Albert Krewinkel

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
]]
PANDOC_VERSION:must_be_at_least '2.11'

local utils = pandoc.utils
local stringify = utils.stringify
local run_json_filter = utils.run_json_filter
local metatype = utils.type or function (value)
  local metatag = type(value) == 'table' and value.t and value.t:gsub('^Meta', '')
  return metatag and metatag ~= 'Map' and metatag or type(value)
end
local all_cites = {}
local doc_meta = pandoc.Meta{}
local refs_div = pandoc.Div({}, pandoc.Attr('refs'))

local citeproc
if utils.citeproc then
  citeproc = utils.citeproc
else
  citeproc = function (doc)
    return run_json_filter(doc, 'pandoc', {
      '--from=json', '--to=json', '--citeproc', '--quiet'
    })
  end
end

local function meta_for_pandoc_citeproc (bibliography)
  local fields = {
    'bibliography', 'references', 'csl', 'citation-style',
    'link-citations', 'citation-abbreviations', 'lang',
    'reference-section-title', 'notes-after-punctuation', 'nocite'
  }
  local new_meta = pandoc.Meta{}
  for _, field in ipairs(fields) do
    new_meta[field] = doc_meta[field]
  end
  new_meta.bibliography = bibliography
  return new_meta
end

local function resolve_doc_citations (doc)
  local meta = doc.meta
  local original_bibliography = meta.bibliography
  local original_suppress_bibliography = meta['suppress-bibliography']
  local bibliography = meta.bibliography
  meta['suppress-bibliography'] = nil
  meta.bibliography = pandoc.MetaList{}
  if metatype(bibliography) == 'table' then
    for _, value in pairs(bibliography) do
      table.insert(meta.bibliography, stringify(value))
    end
  end
  table.insert(doc.blocks, refs_div)
  doc = citeproc(doc)
  table.remove(doc.blocks)
  doc.meta.bibliography = original_bibliography
  doc.meta['suppress-bibliography'] = original_suppress_bibliography
  return doc
end

local function create_topic_bibliography (div)
  local name = div.identifier:match('^refs[-_]?([-_%w]*)$')
  local bibliography = name and (doc_meta['cv-bibliographies'] or {})[name]
  if not bibliography then
    return nil
  end
  local tmp_doc = pandoc.Pandoc(
    { pandoc.Para(all_cites), refs_div },
    meta_for_pandoc_citeproc(bibliography)
  )
  local result = citeproc(tmp_doc)
  div.content = result.blocks[2].content
  div.classes = pandoc.List({
    'references', 'csl-bib-body', 'hanging-indent'
  })
  div.attributes = {
    ['data-entry-spacing'] = '0',
    ['data-line-spacing'] = '2'
  }
  return div
end

return {
  {
    Cite = function (citation)
      all_cites[#all_cites + 1] = citation
    end,
    Meta = function (metadata)
      doc_meta = metadata
    end,
  },
  { Pandoc = resolve_doc_citations },
  { Div = create_topic_bibliography },
}
