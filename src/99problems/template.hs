{-#OPTIONS_GHC -Wall -Werror #-} 

import Test.HUnit

--------------------------------------------------
-- 
--------------------------------------------------




--------------------------------------------------
-- Tests
--------------------------------------------------

-- To execute tests run "runTestTT tests" in the ghci terminal

test :: (_______function_signature_with_type_for_this_test_______) -> Test
test f = TestCase (assertEqual "message"
    _______expected_result_______
    (f _______function_input_______))

testableMethods :: [_______function_signature_______]
testableMethods = [_______each_implementation_to_be_tested______]

tests :: Test
tests = TestList (map _______test_method_______ testableMethods ++
                  map _______other_test_method_______ testableMethods)


