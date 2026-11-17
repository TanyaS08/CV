function Block (el)
  if el.t == "Para" or el.t == "Plain" then
    for k,_ in ipairs(el.content) do
      local name = el.content[k]
      local separator = el.content[k+1]
      local initials = el.content[k+2]

      if name and name.t == "Str" and name.text == "Strydom,"
      and separator and separator.t == "Space"
      and initials and initials.t == "Str" and initials.text:find("^T.") then

          local _,e = initials.text:find("^T.")
          local rest = initials.text:sub(e+1)
          el.content[k] = pandoc.Strong { pandoc.Str("Strydom, T.") }
          el.content[k+1] = pandoc.Str(rest)
          table.remove(el.content, k+2)

      elseif name and name.t == "Str" and name.text == "Strydom*,"
      and separator and separator.t == "Space"
      and initials and initials.t == "Str" and initials.text:find("^T.") then

          local _,e = initials.text:find("^T.")
          local rest = initials.text:sub(e+1)
          el.content[k] = pandoc.Strong { pandoc.Str("Strydom*, T.") }
          el.content[k+1] = pandoc.Str(rest)
          table.remove(el.content, k+2)
      end

    end
  end
  return el
end
