qs_panel <- '
<!-- #########  QUICK START PANEL #########   -->
<div class="col-sm-9">
	<div class="panel-group" id="accordion">
		<div class="panel panel-primary">
			<div class="panel-heading" data-toggle="collapse" data-parent="#accordion" data-target="#collapseOne">
				<h4 class="panel-title accordion-toggle">Manual</h4>
			</div>
			<div id="collapseOne" class="panel-collapse collapse">
				<div class="panel-body">
					
					<h3>Step 1: The initial sample</h3>
					Go to the tab "New study" on the left. 
					
					<ul>
					<li>Decide how many participants you want to collect initially.<br>
					<b>Pro-Tip: You increase your chances of finding a significant effect when you run many studies with few participants, instead of few studies with many participants (Bakker, van Dijk, & Wicherts, 2012)!</b>
					</li>					
					
					<li>Next, decide what the true effect size should be.<br>
					<b>Pro-Tip: For a proper training in p-hacking, always select "0"!</b> Then you can train to squeeze out an effect from nothing - isn`t that cool!?
					</li>
					<li>Next, decide how many potential dependent variables (DVs) you assess. (Technical detail: all DVs correlate to r=.5)<br>
					<b>Pro-Tip: The more DVs you measure, the more you increase the chance of finding something! DV_all is an aggregate of all DVs.</b>
					</li>
					<li>
					Finally, click on the button <code>Run new experiment</code> to collect your sample.
					</li>
					</ul>
					<br>
					Now, take a look at the <i>p</i>-values in the middle pane. For your convenience, significant results are already marked in green, and result that are teetering on the brink of significance (i.e., promising results!) are yellow.
					<ul>
					<li>
					Is it futile, such as p > .60? Meh. Consider to run another conceptual replication. Probably the manipulation did not work, or the DV was not so good. (What a luck that you didn`t run too many subjects on this shitty stimulus set!)
					</li>
					<li>
					But maybe the <i>p</i>-value is in a promising region, say p <.20? Great! That`s a near hit. Are you ready to go to Step 2? <b>Now comes the fun part!</b>
					</li>
					</ul>
					
					
					<h3>Step 2: Polish your <i>p</i>-value</h3>
					Got to the tab "Now: p-hack!". This gives you all the great tools to improve your current study. Here you can fully utilize your data analytic skills and creativity.
					
					<b>Things to try:</b>
					<ul>
					<li>Have you looked at all dependent variables (DVs)? And also their aggregate?</li>
					<li>Have you tried to control for age? Or for gender? Or for both?</li>
					<li>Maybe the effect is only present in one gender. You should try the interaction. (You will find a great post-hoc explanation, why this only works in men. I count on your creativity!)</li>
					<li>Push your result across the 5% boundary by adding 5 or 10 new subjects! (The 5% criterion is arbitrary anyway, isn`t it?)</li>
					<li>Remove outliers! Simply click on a data point in the plot to exclude (or re-include) it from your analysis. This is also very powerful when you look at the interaction with gender: Sometimes a point is an outlier only when you consider genders separately.</li>
					</ul>
					
					Nothing helped to get a significant result? Well, that happens to the best of us. <br>
					Don`t become desperate, and don`t think too long about why that specific study failed. <br><br>
					Now it is important to show even more productivity: Go for the next conceptual rpelication (i.e., go back to Step 1 and collect a new sample, with a new manipulation and a new DV).<br><br>
					<b>Pro-Tip: Never do direct replications (aka. "stupid method repetitions")! </b>
					<ul>
					<li>First, this is only for second-stringers without creative potential.</li>
					<li>Second, <b>direct replications lock the "Now: p-hack" tab! Oh no!</b> With direct replications, you are <b>forced to use the same DV as before</b>, and you cannot choose anymore from several DVs. If you controlled for age in the first study, you would have to control for age in the direct replication as well, etc. <i>All this compulsive, anal-retentive stuff just limits your creative potential.</i></li>
					<li>Instead of conducting a direct replication (which, at n=20, wouldn`t take too long, right?), we rather suggest to write <a href="https://pigee.wordpress.com/2015/04/23/be-your-own-replicator/">long rebuttals</a> about the merits of conceptual replication. You can point to all of the successful conceptual replications you have collected in your study stack! (See Step 3.)</li>
					</ul>
					
					<h3>Step 3: Harvest your accomplishments</h3>
					You found a significant effect? We congratulate you for your creativity and productivity.
					<br><br>
					On the right panel, you can harvest your successes. Simply click on the <code>Save</code> button next to each DV and the current study is saved to your stack, awaiting some additional conceptual replications that show the robustness of the effect.
					<br><br>
					But the challenge continues. Many journals require multiple studies - but that should be no problem for you. Go back to Step 1. Craft a new sample with a significant <i>p</i>-value, and when you have it, save it to your stack. 
					<br><br>
					Four to six studies should make a compelling case for your subtile, counterintuitive, and shocking effects. Honor to whom honor is due: Find the best outlet for your achievements!
					
					
					<h3>Step 4: The nasty part</h3>
					Those were the good times where we could stop here. But some nasty researchers developed tools that are quite powerful in detecting p-hacking. If you click on the <code>Send to p-checker</code> button below your study stack on the right, the saved test statistics are transfered to the <i>p</i>-checker app. Let`s see whether we can detect <i>p</i>-hacking!
					
					<h4>References</h4>
					Bakker, M., van Dijk, A., & Wicherts, J. M. (2012). The rules of the game called psychological science. Perspectives on Psychological Science, 7, 543–554. doi:10.1177/1745691612459060					
				</div>
			</div>
		</div>
	</div>
</div>
'
