/*

import Elm.Kernel.Utils exposing (chr)

*/



// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Parser_tuple3(isGood ? offset : -1, row, col);
});


function _Parser_tuple3(a, b, c)
{
	return { $: '_Tuple3', a: a, b: b, c: c };
}



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(__Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(__Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});



// FIND STRING


var _Parser_findSubString = F6(function(before, smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = before ? newOffset : newOffset + smallString.length

	if (newOffset < 0)
	{
		return _Parser_tuple3(-1, row, col);
	}

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Parser_tuple3(offset, row, col);
});
