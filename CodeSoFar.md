# WORK IN PROGRESS #

The following code will change at my whim. May be updated regularly, may not. Comments/suggestions/modifications welcome - send along to woragrump@gmail.com.

```
@@ REQUIRED FEATURES - READ THIS FIRST!
@@ -
@@ You are going to need a Function Invocation Level of at least 10,000.
@@ -
@@ You are assumed to have an isstaff(dbref) function. The SGP Globals come with one.
@@ -
@@ GAME-SPECIFIC DIFFERENCES
@@ -
@@ This section is likely only interesting to coders, and maybe not even those. I apologize in advance for any system bias I am perceived to be displaying. Read on at your own risk.
@@ -
@@ itemize(<list>, <delim>) - equates to elist(<list>, <phrase>, <delim>) on Rhost. Rhost users may need to create a custom itemize function. There should be one included with the SGP Globals, however. If not, here's mine:
@@ &fn-itemize OBJECT=elist(%0,if(t(%2),%2,and),%1,if(t(%4),%4,%b),if(t(%3),%3,%,))
@@ You will need to, at startup, using your god bit, call the following bit of code:
@@ @function itemize=OBJECT/fn-itemize;
@@ Most people automate this with a @startup command on #1.
@@ -
@@ iter() calls which are required to have no spaces have an output delimiter of "@@" for MUX compatibility. Because of this, I've added edit(input, @@,) to the iters. That's gonna suck for any players who want @@ in their text for some reason, but oh well.
@@ -
@@ Since there's no ibreak() on MUX, I've used first(iter(blah, if(bleh, bloo|<term>|)), |<term>|). Waste of cycles, but easy to convert - just search for the first |<term>| and stick a [ibreak()] in its place, then remove the 'first(' and the ', |<term>|)'. Note that you will have to do this by hand (or at least look before hitting replace all) because first( is used elsewhere in the code. Or you could just leave it as is - not like nowadays we can't afford the extra cycles.
@@ -
@@ streq() has been replaced with not(comp(string1, string2)) for MUX compatibility.
@@ -
@@ Reverse strip (strip only characters NOT in the permitted list) has been replaced by not(comp(strip(input, list of valid characters),))
@@ -
@@ -
@@ -
@@ -
@@ -
@@ -

@create Template DataBase <TDB>=10
@set TDB=HALT

@create Restriction DataBase <RDB>=10
@set RDB=HALT
@parent RDB=TDB

@create Point DataBase <PDB>=10
@set PDB=HALT
@parent PDB=RDB

@create Stat DataBase <SDB>=10
@set SDB=HALT
@parent SDB=PDB

@create Settings DataBase <STDB>=10
@set STDB=HALT
@parent STDB=SDB


@force me=&vS STDB=num(SDB)
@force me=&vR STDB=num(RDB)
@force me=&vP STDB=num(PDB)
@force me=&vT STDB=num(TDB)
@force me=@tel SDB=STDB
@force me=@tel RDB=STDB
@force me=@tel PDB=STDB
@force me=@tel TDB=STDB

&strict_characters STDB=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 !@$^`'",.?_

&regex_characters STDB=[v(strict_characters)]|*#()-+/

&compare_characters STDB=[v(strict_characters)]|*#()<>=&-+/

&reserved_words STDB=Tier Template Greatest Least If Setting Flag CurrentValue Points Pool Group Type Stat Experience Freebies





@create StatDB Functions <SDBF>=10
@set SDBF=HALT INHERIT
@parent SDBF=STDB

@@ Locks

&fn-isstaff SDBF=isstaff(%0)

&fn-canusecg SDBF=and(andflags(%0, Pc), not(cor(hasflag(%0, guest), haspower(%0, guest))))

@@ Layout settings

&fn-stdheader SDBF=%b.[center(< %0 >, 75, -)].

&fn-stdfooter SDBF=%b.[if(t(%0), center(< %0 >, 75, -), repeat(-, 75))].

&fn-threecolumns SDBF=edit(iter(%0, ljust(mid(itext(0), 0, setr(0, if(eq(mod(inum(0), 3), 2), 24, 23))), %q0)[if(eq(mod(inum(0), 3), 0), if(neq(words(%0, if(t(%1), %1, %b)), inum(0)), %R %b), %b%b)], if(t(%1), %1, %b), @@), @@,)

@@ Do not change below this line.

&filter-stats_by_name SDBF=strmatch(extract(v(%0), 2, 1, :), %1*)

&filter-stats_by_type SDBF=not(comp(extract(v(%0), 1, 1, :), %1))

&filter-stats_by_name_and_type SDBF=and(u(filter-stats_by_name, %0, %1), u(filter-stats_by_type, %0, %2))

@@ MATH functions credit Raevnos@PennMUSH and licensed under LGPL here: http://raevnos.pennmush.org/mushcode/funclib.txt

&fn-div SDBF=if(strlen(%1), fdiv(%0, %1), %0)

&fn-parse_div SDBF=fold(fn-div, rest(%0, /), first(%0, /), /)

&fn-mul SDBF=if(strlen(%1), mul(u(fn-parse_div, %0), u(fn-parse_div, %1)), u(fn-parse_div, %0))

&fn-parse_mul SDBF=fold(fn-mul, rest(%0, *), first(%0, *), *)

&fn-sub SDBF=if(strlen(%1), if(%0, sub(u(fn-parse_mul, %0), u(fn-parse_mul, %1)), u(fn-parse_mul, -%1)), u(fn-parse_mul, %0))

&fn-parse_sub SDBF=if(regmatch(%0, \[\*/\]-), u(fn-parse_mul, %0), fold(fn-sub, rest(%0, -), first(%0, -), -))

&fn-add SDBF=if(strlen(%1), add(u(fn-parse_sub, %0), u(fn-parse_sub, %1)), u(fn-parse_sub, %0))

&fn-parse_add SDBF=fold(fn-add, rest(%0, +), first(%0, +), +)

&fn-parse_parens SDBF=if(pos(%(, %0), u(fn-parse_parens, trim(edit(edit(strip(replace(%0, match(%0, \(*\)), u(fn-parse_add, strip(grab(%0, \(*\)), ()))), %b), \\\(, %b\\\(), \\\), \\\)%b))), u(fn-parse_add, %0))

&fn-insert_mul SDBF=if(regmatch(%0, lit(([0-9\)])(\()|(\))([0-9\(])), 0 1 2 3 4), u(fn-insert_mul, edit(%0, %q0, if(%q1, %q1, %q3)*[if(%q2, %q2, %q4)])), %0)

&fn-calc SDBF=u(fn-parse_parens, trim(edit(edit(ulocal(fn-insert_mul, edit(strip(%0, %b), --, +)), \(, %b\(), \), \)%b)))

@@ End MATH functions

&fn-valid_characters SDBF=not(comp(strip(%0, u(%1_characters)),))

&fn-match_statgroup SDBF=match(v(statgroup_names), %0*, |)

&fn-match_pointgroup SDBF=match(v(pointgroup_names), %0*, |)

&fn-match_tier SDBF=match(v(tier_names), %0*, |)

&fn-match_type SDBF=match(v(stat_types), %0*, |)

&fn-is_statgroup SDBF=t(match(v(statgroup_names), %0, |))

&fn-is_pointgroup SDBF=t(match(v(pointgroup_names), %0, |))

&fn-is_tier SDBF=t(match(v(tier_names), %0, |))

&fn-is_type SDBF=t(match(v(stat_types), %0, |))

&fn-is_stat SDBF=t(match(iter(grepi(%vS, stat-*, [if(t(%1), %1)]:%0:), extract(v(itext(0)), 2, 1, :),, :), %0, :))

&fn-find_point SDBF=match(iter(grepi(%vP, tier-*, %0:), extract(v(itext(0)), 1, 3, :),, |), %0, |)

&fn-find_stat SDBF=first(filter(filter-stats_by_name[if(t(%1), _and_type)], grepi(%vS, stat-*, :%0),,, %0, %1,))

&fn-get_stat_name SDBF=extract(v(%0), 2, 1, :)

&fn-get_stat_type SDBF=extract(v(%0), 1, 1, :)

&fn-get_stat_value SDBF=extract(v(%0), 3, 1, :)

&fn-get_stat_requirements SDBF=default(%vR/[edit(%0, stat_, req_)], None)

&fn-get_stat_substats SDBF=extract(v(%0), 4, 9999, :)

&fn-get_type_name SDBF=extract(v(stat_types), u(fn-match_type, %0), 1, |)

&fn-get_tier_name SDBF=extract(v(tier_names), %0, 1, |)

&fn-get_statgroup_name SDBF=extract(v(statgroup_names), %0, 1, |)

&fn-get_pointgroup_name SDBF=extract(v(pointgroup_names), %0, 1, |)

&fn-list_stats SDBF=filter(filter-stats_by_type, grepi(%vS, stat-*, %0),,, %0,)

&fn-is_valid_number SDBF=and(isnum(%0), gte(%0, 0), lte(%0, 10))

&fn-is_valid_point_number SDBF=and(isnum(%0), gte(%0, 0))

&fn-is_valid_point_list SDBF=not(t(ladd(iter(%0, not(u(fn-is_valid_point_number, itext(0)))))))

&fn-check_parens SDBF=eq(words(A%0A, %(), words(A%0A, %)))

&fn-max SDBF=max(%0, %1)

&fn-min SDBF=min(%0, %1)

&fn-if SDBF=if(%0, first(%1, |), rest(%1, |))

@@ Priorities:
@@ 1. Perform this process on the conditional fields of the below.
@@    * Greatest:Item~Item~Item
@@    * Least:Item~Item
@@    * If:Condition~Result[~Condition~Result]~False Value
@@ 2. Translate the following into numbers.
@@    * Setting:OneWord
@@    * Flag:OneWord
@@    * Stats
@@ 3. Execute each parenthetical statement separately.
@@ 4. Execute any multiplication.
@@ 5. Execute any division.
@@ 6. Execute any addition.
@@ 7. Execute any subtraction.
@@ 8. If conditional statements are included, evaluate them to 0 or 1.

&fn-is_token_char SDBF=case(%0, %(, 1, %), 1, *, 1, /, 1, +, 1, -, 1, &, 1, |, 1, <, 1, >, 1, 0)

&fn-eval_greatest SDBF=fold(fn-max, iter(%0, ulocal(fn-calc, edit(ulocal(fn-tokenize, itext(0)), #, %b)), ~, ~), 0, ~)

&fn-eval_least SDBF=fold(fn-min, iter(%0, ulocal(fn-calc, edit(ulocal(fn-tokenize, itext(0)), #, %b)), ~, ~), 9999, ~)

&fn-eval_if SDBF=%0

&fn-eval_setting SDBF=if(not(isnum(setr(0, default(%qP/%0, 0)))), 1, %q0)

&fn-eval_flag SDBF=hasflag(%qP, %0)

&fn-eval_stat SDBF=if(t(setr(0, trim(%0))), if(t(setr(0, switch(%q0, *:*, u(fn-find_stat, rest(%q0, :), first(%q0, :)), u(fn-find_stat, %q0)))), extract(default(%qP/_%q0, 0:0:0):0:0:0, 3, 1, :), 0))

&fn-eval_token SDBF=if(isnum(%0), %0, switch(%0, Setting:*, u(fn-eval_setting, rest(%0, :)), Flag:*, u(fn-eval_flag, rest(%0, :)), u(fn-eval_stat, %0)))

&fn-eval_tokens SDBF=if(u(fn-is_token_char, %0), if(t(setr(8, u(fn-eval_token, %q8))), setq(9, %q9#%q8#%0), setq(9, %q9#%q8#%0))[setq(8,)], setq(8, %q8%0))

&fn-tokenize SDBF=setq(8, setq(9,))[foreach(fn-eval_tokens, setr(W, %0))][setq(9, trim(squish(strip(if(t(setr(8, u(fn-eval_token, %q8))), %q9#%q8, %q9#0), %b), #), b, #))]%q9[setq(8, setq(9,))]

&fn-run_function SDBF=switch(%0, Greatest:*, u(fn-eval_greatest, rest(%0, :)), Least:*, u(fn-eval_least, rest(%0, :)), If:*, u(fn-eval_if, rest(%0, :)), ulocal(fn-calc, edit(ulocal(fn-tokenize, %0), #, %b)))

&fn-capwords SDBF=iter(edit(lcstr(%0),_,%b), capstr(itext(0)))

&fn-find_input_type SDBF=switch(%0, *:*, switch(first(%0, :), Pool, Pool, Group, Group, Type, Type, Stat), if(t(match(Freebies Experience, %0)), Pool, if(u(fn-is_pointgroup, u(fn-match_pointgroup, %0)), Group, if(u(fn-is_type, %0), Type, if(cor(strmatch(Freebies, %0*), strmatch(Experience, %0*)), Pool, if(u(fn-match_pointgroup, %0), Group, if(u(fn-match_type, %0), Type, Stat)))))))

&fn-parse_list SDBF=edit(iter(%0, switch(itext(0), \\*, %1, #, if(u(fn-is_valid_number, %1), %1), %1*, itext(0)), |, @@), @@,)

&fn-parse_stat_name SDBF=u(fn-capwords, switch(%0, * \(*\), first(%0, \()\([u(fn-parse_list, first(rest(%0, \()\)))]\)[rest(%0, \))], *|*, u(fn-parse_list, %0), #, if(u(fn-is_valid_number, %1), %1), %1*, u(fn-get_stat_name, %2), case(%0, *, %1)))

&fn-list_valid_values SDBF=switch(%0, #, any number between 1 and 10 %(most stats expect 1-5%), *|*, itemize(edit(edit(%0, #, any number between 1 and 10 %(most stats expect 1-5%)), *, any text that does not include invalid characters), |, or)., if(strmatch(%0, *\**), edit(%0, *, any text that does not include invalid characters), %0))

&fn-is_valid_value SDBF=switch(%0, #, u(fn-is_valid_number, %1), *|*, t(edit(iter(%0, or(strmatch(itext(0), %1*), and(strmatch(itext(0), *\**), strmatch(%1, itext(0))), and(strmatch(itext(0), #), u(fn-is_valid_number, %1))), |, @@), @@,)), strmatch(%0, %1*))





@create Chargen Commands <CGC>=10
@set CGC=INHERIT
@parent CGC=SDBF
@lock CGC=fn-canusecg/1
@fail CGC=You are not currently permitted to use the character generator. This may be because you're a guest character, or because staff would like you to talk to them before you enter character generation. See the news files or talk to staff to find out how to get set up to use CG!

&cmd-+cg CGC=$+cg:@pemit %#=u(fn-stdheader, Chargen Info)%R %bYour next step should be: %R[u(fn-stdfooter, Type +cg/help for a list of chargen commands.)];

&cmd-+cg/help CGC=$+cg/help:@force %#=+cghelp;

&cmd-+cghelp CGC=$+cghelp:@pemit %#=u(fn-stdheader, Chargen Quick Reference)%R %bAvailable commands are:%R[space(4)]+cg - Suggest your next step!%R[space(4)]+cg Stat - Display information about a stat.%R[space(4)]+cg Stat=Value - Set a stat.%R[space(4)]+cg Templates - List available templates.%R[space(4)]+cg Template=template - Apply a template. Will ask if you're sure.%R[space(4)]+cg/check - Check your sheet for approval.%R[space(4)]+cg/check Stat - Check whether you meet the prerequisites of a stat.%R[space(4)]+cg/complete - Finish chargen!%R[space(4)]+cg/wipe - Wipe your stats. Will ask if you're sure.%R[space(4)]+cg/wipe stat type - Wipe only stats of a particular type. Will ask first.%R%TExample: +cg/wipe Attributes%R[u(fn-stdfooter)];
+cghelp

@@ %qT = Type of Stat
@@ %qS = Stat Name
@@ %qU = Pretty stat name
@@ %qF = Stat Location (Filed under)
@@ %qO = Old stat value
@@ %qV = Value to set stat to
@@ %qM = TeMporary value
@@ %qP = Possible values
@@ %qR = Requirements
&cmd-setvalue CGC=$+cg *:@switch/first %0=Template=*,{},*=*,{@switch null(switch(%0, *:*=*, setq(T, first(%0, :)) [setq(S, first(rest(%0, :), =))], *=*, setq(S, first(%0, =))))[setq(V, rest(%0, =))][setr(E, if(t(%qT), if(not(t(u(fn-match_type, %qT))), Could not find the stat type %qT.%b, setq(T, u(fn-get_type_name, %qT))))[if(t(%qS), if(not(t(setr(F, u(fn-find_stat, %qS, %qT)))), Could not find the stat "%qS"[if(t(%qT), %bwith the stat type "%qT")].%b, setq(U, u(fn-parse_stat_name, setr(M, u(fn-get_stat_name, %qF)), %qS, %qF))[setq(S, %qM)]))][if(not(u(fn-valid_characters, %qU, strict)), You must include valid characters for the value of %qU. Your invalid characters are as follows: [strip(%qU, v(strict_characters))]%b)][if(not(u(fn-valid_characters, %qV, strict)), You must include valid characters for the value of %qU. Your invalid characters are as follows: [strip(%qV, v(strict_characters))]%b)])][if(strmatch(%qE, ), if(not(t(%qT)), setq(T, u(fn-get_stat_type, %qF)))[setr(E, if(not(u(fn-is_valid_value, setr(P, u(fn-get_stat_value, %qF)), %qV)), The value you entered (%qV) does not match with the list of permitted values for %qU. You must have a value of [u(fn-list_valid_values, %qP)] for this stat.%b))])]=,{&_%qF %#=%qT:%qU:%qV[if(t(setr(D, extract(xget(%#, _%qF), 4, 9999, :))), :%qD)]; @pemit %#=You set your %qT %qU to %qV.;},{@pemit %#=ERROR > %qE;};},{@force %#=+ref %0;};






@create StatDB Commands <SDBC>=10
@set SDBC=INHERIT
@parent SDBC=SDBF

&cmd-+ref SDBC=$+ref:@pemit %#=u(fn-stdheader, Stat Reference)%R Available stat types:%R%R%b [ulocal(fn-threecolumns, v(stat_types), |)]%R%R Type +ref Stat Type or Stat Name for more.%R[u(fn-stdfooter)];

&cmd-statdata SDBC=$+ref *:@switch setr(E, if(and(not(setr(S, t(setr(F, u(fn-find_stat, %0))))), not(t(u(fn-match_type, %0)))), Could not find the stat or stat type "%0".%b))=,{@switch %qS=1,{@pemit %#=u(fn-stdheader, u(fn-get_stat_name, setr(F, u(fn-find_stat, %0))) Details)%R %bType: [u(fn-get_stat_type, %qF)]%R %bPossible values: [switch(setr(V, u(fn-get_stat_value, %qF)), \\\\*, Any text that does not include invalid characters., #, Any number between 1 and 10 %(most stats expect 1-5%), *|*, [itemize(edit(edit(%qV, #, any number between 1 and 10 %(most stats expect 1-5%)), *, any text that does not include invalid characters), |, or)]., %qV)]%R %bRequirements: [setr(R, u(fn-get_stat_requirements, %qF))][switch(%qR, None,, if(u(fn-test_stat_requirements, %qF, %#), %bYou meet these requirements., %bYou do not meet these requirements.))]%R[u(fn-stdfooter)];},{@pemit %#=u(fn-stdheader, setr(T, u(fn-get_type_name, %0)) Stats)[if(t(setr(R, u(fn-list_stats, %qT))), %R %b[ulocal(fn-threecolumns, iter(%qR, extract(v(itext(0)), 2, 1, :),, |), |)],%R%R %bThis stat type currently has no stats.%R)]%R[u(fn-stdfooter, +ref Stat Name for more information.)];};},{@pemit %#=ERROR > %qE;};





@create StatDB Staff Commands <STSC>=10
@set STSC=INHERIT
@parent STSC=SDBF
@lock STSC=fn-isstaff/1

&cmd-newtype STSC=$+cg/new *:@switch/first %0=*:*=*,{},{@switch setr(E, if(not(u(fn-valid_characters, %0, strict)), You must include valid characters for the stat type. Your invalid characters are as follows: [strip(%0, u(strict_characters))]%b, if(u(fn-is_type, %0), %0 is already a stat type.%b)))=,{&stat_types %vS=[if(t(v(stat_types)), v(stat_types)|%0, %0)]; @pemit %#=Added your stat type "%0".;},{@pemit %#=PARSE ERROR > %qE;};};

&cmd-newstat STSC=$+cg/new *:@switch/first %0=*:*=*,{@switch setr(E, if(not(u(fn-is_type, setr(T, first(%0, :)))), %qT is not a valid stat type.%b, setq(T, u(fn-get_type_name, %qT)))[if(not(u(fn-valid_characters, setr(S, first(after(%0, :), =)), regex)), You must include valid characters for the stat name. Your invalid characters are as follows: [strip(%qS, u(regex_characters))]%b)][if(not(u(fn-valid_characters, setr(V, rest(%0, =)), regex)), You must include valid characters for the stat value. Your invalid characters are as follows: [strip(%qV, u(regex_characters))]%b)][if(u(fn-is_stat, %qS, %qT), %qS is already a %qT stat.%b)])=,{&stat-[setr(C, add(default(%vS/stat_count, 0), 1))] %vS=%qT:%qS:%qV; &stat_count %vS=%qC; @pemit %#=Added the %qT stat "%qS" with a possible value of "%qV".;},{@pemit %#=PARSE ERROR > %qE;};}

&cmd-newstatgroup STSC=$+cg/statgroup *:@switch/first %0=*=*,{},{@switch setr(E, if(not(u(fn-valid_characters, %0, strict)), You must include valid characters for the new stat group. Your invalid characters are as follows: [strip(%0, u(strict_characters))]%b, if(u(fn-is_statgroup, %0), %0 is already a stat group name.%b)))=,{&statgroup_names %vS=[if(t(v(statgroup_names)), v(statgroup_names)|%0, %0)]; @pemit %#=Added your new stat group "%0".;},{@pemit %#=PARSE ERROR > %qE;};};

&cmd-newpointgroup STSC=$+cg/pointgroup *:@switch/first %0=*=*,{},{@switch setr(E, if(not(u(fn-valid_characters, %0, strict)), You must include valid characters for the new point group. Your invalid characters are as follows: [strip(%0, u(strict_characters))]%b, if(u(fn-is_pointgroup, %0), %0 is already a point group name.%b)))=,{&pointgroup_names %vP=[if(t(v(pointgroup_names)), v(pointgroup_names)|%0, %0)]; @pemit %#=Added your new point group "%0".;},{@pemit %#=PARSE ERROR > %qE;};};

&cmd-groupstats STSC=$+cg/statgroup *=*:@switch/first setr(E, if(not(t(u(fn-is_statgroup, %0))), Could not find a stat group named "%0".%b, edit(iter(%1, if(not(u(fn-find_stat, itext(0))), Could not find the stat "[itext(0)]".%b), |, @@), @@, )[setq(G, u(fn-match_statgroup, %0))]))=,{&stat_group_%qG %vS=[iter(%1, u(fn-get_stat_type, setr(F, u(fn-find_stat, itext(0)))):[u(fn-get_stat_name, %qF)], |, |)]; @pemit %#=You set the [u(fn-get_statgroup_name, %qG)] stat group to: [itemize(iter(v(stat_group_%qG), rest(itext(0), :), |, |), |)].;},{@pemit %#=PARSE ERROR > %qE;};

&cmd-grouppoints STSC=$+cg/pointgroup *=*:@switch/first setr(E, if(not(t(u(fn-is_pointgroup, %0))), Could not find a point group named "%0".%b, edit(iter(%1, if(not(u(fn-match_statgroup, itext(0))), Could not find the stat group "[itext(0)]".%b), |, @@), @@, )[setq(G, u(fn-match_pointgroup, %0))]))=,{&point_group_%qG %vP=[iter(%1, u(fn-get_statgroup_name, u(fn-match_statgroup, itext(0))), |, |)]; @pemit %#=You set the [u(fn-get_pointgroup_name, %qG)] point group to: [itemize(v(point_group_%qG), |)].;},{@pemit %#=PARSE ERROR > %qE;};

&cmd-newtier STSC=$+cg/tier *:@switch/first setr(E, if(not(u(fn-valid_characters, %0, strict)), You must include valid characters for the new tier. Your invalid characters are as follows: [strip(%0, u(strict_characters))]%b, if(u(fn-is_tier, %0), %0 is already a tier name.%b)))=,{&tier_names %vP=[if(t(v(tier_names)), v(tier_names)|%0, %0)]; @pemit %#=Added your new tier "%0".;},{@pemit %#=PARSE ERROR > %qE;};

@@ %0: Tier
@@ %1: Pool, Group, Type, or Stat
@@ %2: Number, List of Numbers, or Stat
@@ %qP: Point Tier
@@ %qT: Stat type
@@ %qS: Stat name
&cmd-points STSC=$+cg/points */*=*:@switch/first setr(E, if(not(t(setr(P, u(fn-match_tier, %0)))), Could not find the tier "%0".%b, setq(P, u(fn-get_tier_name, %qP))))=,{@switch/first u(fn-find_input_type, %1)[setq(S, switch(%1, *:*, rest(%1, :), %1))]=Pool,{@switch/first setr(E, if(not(t(setr(M, match(Freebies Experience, setr(T, %qS)*)))), %qT is not a valid pool type. Valid pool types are "Freebies" and "Experience".%b, setq(T, extract(Freebies Experience, %qM, 1)))[if(cor(not(isnum(%2)), lt(%2, 0)), You must enter a number greater than or equal to 0 for the point value of a pool.%b)])=,{@switch/first setr(M, u(fn-find_point, %qP:Pool:%qT))=0,{&tier-[setr(C, add(default(%vP/tier_count, 0), 1))] %vP=%qP:Pool:%qT:%2; &tier_count %vP=%qC;},{&tier-%qM %vP=%qP:Pool:%qT:%2;}; @pemit %#=Set the %qT points available to %2 for the %qP tier.;},{@pemit %#=PARSE ERROR > %qE;};},Group,{@switch/first setr(E, if(not(setr(M, u(fn-match_pointgroup, %qS))), %qS is not a valid point group.%b, setq(T, u(fn-get_pointgroup_name, %qM))[if(neq(words(%2), setr(N, words(v(point_group_%qM), |))), %qT has %qN groups of stats. You must have exactly %qN point values to spread across them. You have [words(%2)].%b)])[edit(iter(%2, if(cor(not(isnum(itext(0))), lt(itext(0), 0)), itext(0) is not a number or is less than zero.%b),, @@), @@,)])=,{@switch/first setr(M, u(fn-find_point, %qP:Group:%qT))=0,{&tier-[setr(C, add(default(%vP/tier_count, 0), 1))] %vP=%qP:Group:%qT:%2; &tier_count %vP=%qC;},{&tier-%qM %vP=%qP:Group:%qT:%2;}; @pemit %#=Set the points available for the %qT stat group to %2 for the %qP tier.;},{@pemit %#=PARSE ERROR > %qE;};},Type,{@switch/first setr(E, if(not(u(fn-is_type, setr(T, %qS))), %qT is not a valid stat type.%b, setq(T, u(fn-get_type_name, %qT)))[if(cor(not(isnum(%2)), lt(%2, 0)), You must enter a number greater than or equal to 0 for the point value of a stat type.%b)])=,{@switch/first setr(M, u(fn-find_point, %qP:Type:%qT))=0,{&tier-[setr(C, add(default(%vP/tier_count, 0), 1))] %vP=%qP:Type:%qT:%2; &tier_count %vP=%qC;},{&tier-%qM %vP=%qP:Type:%qT:%2;}; @pemit %#=Set the points available for any %qT to %2 for the %qP tier.;},{@pemit %#=PARSE ERROR > %qE;};},{@switch/first setr(E, switch(%1, *:*, if(not(u(fn-is_type, setr(T, first(%1, :)))), %qT is not a valid stat type.%b, setq(T, u(fn-get_type_name, %qT))))[if(not(t(setr(F, u(fn-find_stat, %qS, %qT)))), Could not find the %qT stat "%qS".%b, setq(S, u(fn-get_stat_name, %qF))[setq(T, u(fn-get_stat_type, %qF))])][if(cor(not(isnum(%2)), lt(%2, 0)), You must enter a number greater than or equal to 0 for the point value of a stat.%b)])=,{@switch/first setr(M, u(fn-find_point, %qP:%qT:%qS))=0,{&tier-[setr(C, add(default(%vP/tier_count, 0), 1))] %vP=%qP:%qT:%qS:%2; &tier_count %vP=%qC;},{&tier-%qM %vP=%qP:%qT:%qS:%2;}; @pemit %#=Set the points available for the %qT %qS to %2 for the %qP tier.;},{@pemit %#=PARSE ERROR > %qE;};};},{@pemit %#=ERROR > %qE;};

&cmd-nukecg STSC=$+cg/nuke:@wipe %vS; @wipe %vR; @wipe %vP; @wipe %vT; @pemit %#=Wiped all stats.



@@ Run these to create your own NWoD DB. Customize at will.

+cg/tier Default

+cg/new Attribute

+cg/new Attribute:Strength=#
+cg/new Attribute:Presence=#
+cg/new Attribute:Intelligence=#
+cg/new Attribute:Dexterity=#
+cg/new Attribute:Manipulation=#
+cg/new Attribute:Wits=#
+cg/new Attribute:Stamina=#
+cg/new Attribute:Composure=#
+cg/new Attribute:Resolve=#

+cg/statgroup Physical Attributes
+cg/statgroup Social Attributes
+cg/statgroup Mental Attributes

+cg/pointgroup Attributes

+cg/statgroup Physical Attributes=Strength|Dexterity|Stamina
+cg/statgroup Social Attributes=Presence|Manipulation|Composure
+cg/statgroup Mental Attributes=Intelligence|Wits|Resolve

+cg/pointgroup Attributes=Physical Attributes|Social Attributes|Mental Attributes

+cg/points Default/Attributes=6 4 3


+cg/new Skill

+cg/new Skill:Academics=#
+cg/new Skill:Athletics=#
+cg/new Skill:Animal Ken=#
+cg/new Skill:Computer=#
+cg/new Skill:Brawl=#
+cg/new Skill:Empathy=#
+cg/new Skill:Crafts=#
+cg/new Skill:Drive=#
+cg/new Skill:Expression=#
+cg/new Skill:Investigation=#
+cg/new Skill:Firearms=#
+cg/new Skill:Intimidation=#
+cg/new Skill:Medicine=#
+cg/new Skill:Larceny=#
+cg/new Skill:Persuasion=#
+cg/new Skill:Occult=#
+cg/new Skill:Stealth=#
+cg/new Skill:Socialize=#
+cg/new Skill:Politics=#
+cg/new Skill:Survival=#
+cg/new Skill:Streetwise=#
+cg/new Skill:Science=#
+cg/new Skill:Weaponry=#
+cg/new Skill:Subterfuge=#

+cg/statgroup Physical Skills
+cg/statgroup Social Skills
+cg/statgroup Mental Skills

+cg/pointgroup Skills

+cg/statgroup Physical Skills=Academics|Computer|Crafts|Investigation|Medicine|Occult|Politics|Science
+cg/statgroup Social Skills=Athletics|Brawl|Drive|Firearms|Larceny|Stealth|Survival|Weaponry
+cg/statgroup Mental Skills=Animal Ken|Empathy|Expression|Intimidation|Persuasion|Socialize|Streetwise|Subterfuge

+cg/pointgroup Skills=Physical Skills|Social Skills|Mental Skills

+cg/points Default/Skills=11 7 4

+cg/new Specialty

+cg/new Specialty:Academics (*)=1
+cg/new Specialty:Athletics (*)=1
+cg/new Specialty:Animal Ken (*)=1
+cg/new Specialty:Computer (*)=1
+cg/new Specialty:Brawl (*)=1
+cg/new Specialty:Empathy (*)=1
+cg/new Specialty:Crafts (*)=1
+cg/new Specialty:Drive (*)=1
+cg/new Specialty:Expression (*)=1
+cg/new Specialty:Investigation (*)=1
+cg/new Specialty:Firearms (*)=1
+cg/new Specialty:Intimidation (*)=1
+cg/new Specialty:Medicine (*)=1
+cg/new Specialty:Larceny (*)=1
+cg/new Specialty:Persuasion (*)=1
+cg/new Specialty:Occult (*)=1
+cg/new Specialty:Stealth (*)=1
+cg/new Specialty:Socialize (*)=1
+cg/new Specialty:Politics (*)=1
+cg/new Specialty:Survival (*)=1
+cg/new Specialty:Streetwise (*)=1
+cg/new Specialty:Science (*)=1
+cg/new Specialty:Weaponry (*)=1
+cg/new Specialty:Subterfuge (*)=1

+cg/restrict Specialty:Academics (*)=Skill:Academics = 1
+cg/restrict Specialty:Athletics (*)=Skill:Athletics = 1
+cg/restrict Specialty:Animal Ken (*)=Skill:Animal Ken = 1
+cg/restrict Specialty:Computer (*)=Skill:Computer = 1
+cg/restrict Specialty:Brawl (*)=Skill:Brawl = 1
+cg/restrict Specialty:Empathy (*)=Skill:Empathy = 1
+cg/restrict Specialty:Crafts (*)=Skill:Crafts = 1
+cg/restrict Specialty:Drive (*)=Skill:Drive = 1
+cg/restrict Specialty:Expression (*)=Skill:Expression = 1
+cg/restrict Specialty:Investigation (*)=Skill:Investigation = 1
+cg/restrict Specialty:Firearms (*)=Skill:Firearms = 1
+cg/restrict Specialty:Intimidation (*)=Skill:Intimidation = 1
+cg/restrict Specialty:Medicine (*)=Skill:Medicine = 1
+cg/restrict Specialty:Larceny (*)=Skill:Larceny = 1
+cg/restrict Specialty:Persuasion (*)=Skill:Persuasion = 1
+cg/restrict Specialty:Occult (*)=Skill:Occult = 1
+cg/restrict Specialty:Stealth (*)=Skill:Stealth = 1
+cg/restrict Specialty:Socialize (*)=Skill:Socialize = 1
+cg/restrict Specialty:Politics (*)=Skill:Politics = 1
+cg/restrict Specialty:Survival (*)=Skill:Survival = 1
+cg/restrict Specialty:Streetwise (*)=Skill:Streetwise = 1
+cg/restrict Specialty:Science (*)=Skill:Science = 1
+cg/restrict Specialty:Weaponry (*)=Skill:Weaponry = 1
+cg/restrict Specialty:Subterfuge (*)=Skill:Subterfuge = 1

+cg/new Pool
+cg/new Merit
+cg/new Contract
+cg/new Derangement
+cg/new Specialty
+cg/new Basic

+cg/new Basic:Race=Mortal|Mage|Changeling|Vampire|Werewolf
+cg/new Merit:Status (Police|Local|*)=#


```