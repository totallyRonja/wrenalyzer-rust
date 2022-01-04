import "test/one" for Test, Dict, Blob
import "definition" for Str as CoolString
import "luxe: color" for White as UwU
import "luxe: anim/sprite" for Sprite

0x666
"test text"


#doc= "cool class, inherits from Test"
#number=9
class Other is Test{
	static test{
		var ba: Test = Test.new()
		ba.prop()
		CoolString.hex(69)
		Blob.prop

		var x = Dict["aaa"]
	}

	#doc="""
		Beep Boop test docs
	"""
	static test2(argument: Num, argument2: Test){
		var a = argument
		var b = a + argument2.prop
		
		__mew = ":3"
	}

	static func(args: List){
		
	}

	thing{_cool}
	thing=(v){_cool=v}
	other{_other}

	static mew{__mew}

	construct new(){
		import "luxe: color" for Color as Colour
		
	}
}

var a = Other.new()
a.test_property
a.thing

var map = ["hot", "hot", "cheese"]
