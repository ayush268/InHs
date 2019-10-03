% Test Case 1
% Variable not in scope
local Bob in
   local Alice in
      skip
   end
   Bob = Alice
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 2
% Unification error due to incompatible
% types/values
local Bob in
   local Alice in
      Alice = 5
      Bob = 7
      Alice = Bob
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 3
% Unification failed due to records
% not matching (arity match fails)
local Eve in
   local Doug in
      local Charles in
	 local Bob in
	    local Alice in
	       Alice = 12(1:Bob, 2:Charles)
	       Doug = 12(1:Eve)
	    end
	 end
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 4
% Conditional expression is not literal
local Alice in
   local Bob in
      local Charles in
	 local X in
	    Alice = 100(1:Bob, 2:Charles)
	    if Alice then X = 1
	    else X = 2
	    end
	 end
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 5
% Pattern in a case statement is not record
local X in
   local Y in
      case X
      of 100 then Y = 100
      else Y = 0 end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 6
% Procedure Call (Type is not a closure)
local X in
   local Y in
      X = 12
      Y = 13
      {X Y}  % X is a literal here
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 7
% Procedure Call (Insufficient arguments)
local X in
   local Y in
      X = proc {$ A B}
	     B = A
	  end
      Y = 10
      {X Y}
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
