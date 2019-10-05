% Test Case 1
% Multiple statements using 'skip' and 'var'
skip
local X in
   skip
end
local Y in
   local X in
      skip
   end
end
local Z in
   local Y in
      local X in
	 skip
      end
   end
end
local X in
   skip
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 2
% Unification of unbound variables
% Bind Ident
local Bob in
   local Alice in
      Alice = Bob
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 3
% Unification of bound and unbound variables
% Bind Ident + Bind Value
local Bob in
   local Alice in
      Alice = 5
      Alice = Bob
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 4
% Unification of 3 unbound variables
local Alice in
   local Bob in
      local Charles in
	 Bob = Alice
	 Charles = Bob
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 5
% Unification of bound variables
% Bind Value + Bind Ident

local Bob in
   local Alice in
      Alice = 5
      Bob = 5
      Alice = Bob
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 6
% Assignment (Binding) of a Value to a Record
local Charles in
   local Bob in
      local Alice in
	 Alice = 12(1:Bob, 2:Charles)
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 7
% Bind Value to a Closure/Procedure (CE)
local Bob in
   Bob = proc {$ A B}
	    skip
	 end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 8
% Bind Value to a Closure (with Free Variables)
local Charles in
   local Bob in
      local Alice in
	 Charles = proc {$ Bob}
		      Alice = Bob
		   end
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 9
% Successful Unification of records
local Frank in
   local Eve in
      local Doug in
	 local Charles in
	    local Bob in
	       local Alice in
		  Alice = 12(1:Bob, 2:Charles)
		  Doug = 12(1:Eve, 2:Frank)
		  Alice = Doug
	       end
	    end
	 end
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 10
% Conditional Statements (Taking True branch)
local Y in
   local X in
      X = 1     % Non-Zero Value
      if X then Y = X
      else Y = 100
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 11
% Conditional Statements (Taking False branch)
local Y in
   local X in
      X = 0
      if X then Y = X
      else Y = 100
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 12
% Match (Case) statement (Pattern matched)
local A in
   local B in
      local X in
	 local Y in
	    X = 15(10:A, 12:B)
	    case X
	    of 15(10:m, 12:n) then Y=1
	    else Y = B
	    end
	 end
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 13
% Match (Case) statement (Pattern NOT matched)
local A in
   local B in
      local X in
	 local Y in
	    X = 15(10:A, 12:B)
	    case X
	    of 15(10:m, 10:n) then Y=M
	    else Y = B
	    end
	 end
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 14
% Procedure Call (Without free variables)

local F in
   local A in
      local B in
	 F = proc {$ X Y}
		Y = X
	     end
	 B = 1
	 {F B A}
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test Case 15
% Procedure Call (With free variables)

local F in
   local A in
      local B in
	 local C in
	    F = proc {$ X Y}
		   Y = X
		   C = Y
		end
	    C = 10
	 end
	 {F A B}
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
