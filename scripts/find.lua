-- Find command for Svlad
-- (c) Severak 2016

if not cmd_find then
	function cmd_find()
		local subj, maxr, maxc, r, c
		subj = svlad.prompt('Find:', '')
		maxr, maxc = svlad.get_max()
		for r=0, maxr do
			for c=0, maxc do
				if svlad.get(r, c)==subj then
					svlad.focus(r, c)
					return
				end
			end
		end
		svlad.alert('"' .. subj ..  '" not found.')
	end

	svlad.menu_add('Find text', 'cmd_find')
end