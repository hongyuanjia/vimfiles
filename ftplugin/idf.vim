function! IDFFolds()
    let thisline = getline(v:lnum)
    " Define all regex used.
    let regex_class = '^\(! \)*!- \{3}=\{11} \{2}ALL OBJECTS IN CLASS: \(.*\) =\{11}' 
    let regex_macro = '^\(! \)*#\(#\(include\|fileprefix\|includesilent\|nosilent\|if\|ifdef\|ifndef\|elseif\|else\|endif\|def\|enddef\|def1\|set1\|list\|nolist\|show\|noshow\|showdetail\|noshowdetail\|expandcomment\|traceback\|notraceback\|write\|nowrite\|symboltable\|clear\|reverse\|!\)\|eval\|[\)'
    let regex_object = '^\(\! \)*\s*\([A-Z].*\),$'
    let regex_blank = '^\s*$'
    let regex_field = '^\(! \)*\s*\(\S.*\)[,;]\s*!\s*-\s*.*$'

    if thisline =~ regex_class
        return ">1"
    elseif thisline =~ regex_object
        let nextline = getline(v:lnum + 1)
        if nextline =~ regex_field
            return ">2"
        else
            return "-1"
        endif
    elseif thisline =~ regex_blank
        return "-1"
    elseif thisline =~ regex_macro
        return "-1"
    else
        return "="
    endif
endfunction
setlocal foldmethod=expr
setlocal foldexpr=IDFFolds()

function! IDFFoldText()
    " Define all regex used.
    let regex_class = '^\(! \)*!- \{3}=\{11} \{2}ALL OBJECTS IN CLASS: \(.*\) =\{11}' 
    let regex_object = '^\(\! \)*\s*\([A-Z].*\),$'
    let regex_field = '^\(! \)*\s*\(\S.*\)[,;]\s*!\s*-\s*.*$'

    let line = getline(v:foldstart)
    let level = v:foldlevel
    let linenext = getline(v:foldstart+1)
    let lineoutput = getline(v:foldstart+2)
    if level == 1
        let class = substitute(line, regex_class, '\1\L\u\2','g')
    elseif level == 2
        if linenext =~ '^\(! \)*\s*\(\S.*\),.*!\s*-\s*Key Value$'
            let class = substitute(line, regex_object, '\1\2: ', 'g'). substitute(linenext, regex_field, '\1\2', 'g'). '_' . substitute(lineoutput, regex_field, '\2', 'g')
        else
            let class = substitute(line, regex_object, '\1\2: ', 'g'). substitute(linenext, regex_field, '\2', 'g')
        endif
    endif
    let lines_count = v:foldend - v:foldstart + 1
    let lines_count_text = '| ' . printf("%10s", lines_count . ' lines') . ' |'
    let foldchar = matchstr(&fillchars, 'fold:\zs.')
    let foldtextstart = strpart(repeat('▼', v:foldlevel) . repeat('―', v:foldlevel*1) . class, 0, (winwidth(0)*2)/3)
    let foldtextend = lines_count_text . repeat(foldchar, 8)
    let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
    return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction
setlocal foldtext=IDFFoldText()
