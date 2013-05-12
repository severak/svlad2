---
-- Multiplication table
---

svlad.set(0,0,'MULTIPLICATION')

for x=1,20 do
	svlad.set(0,x,x)
end

for y=1,20 do
	svlad.set(y,0,y)
end

for x=1,20 do
	for y=1,20 do
		svlad.set(x,y,x*y)
	end
end