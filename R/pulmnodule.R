pulmnodule <- function(age = 0, diameter = 0, cancer = 0, smoker = 0, spiculation = 0, location = 0, geo_region = 'none'){  
  if(age < 1){
    stop("Tell me your age!")
  }
  message = '';
  prob = eval(-6.8272 + (age * 0.0391) + (diameter * 0.1274) + (cancer * 1.3388) + (smoker * 0.7917) + (location * 0.7838) + (1.0407 * spiculation))
  prob = round(100 * exp(prob)/(1 + exp(prob)),1)
  citations = "1. Swensen SJ, Silverstein MD, Ilstrup DM, Schleck CD, Edell ES: The probability of malignancy in solitary pulmonary nodules. Application to small radiologically indeterminate nodules. Arch Intern Med 157. (8): 849-855.1997;"
  citations = paste(citations,"2. Herder GJ, van Tinteren H, Golding RP, et al: Clinical prediction model to characterize pulmonary nodules: validation and added value of 18F-fluorodeoxyglucose positron emission tomography. Chest 128. (4): 2490-2496.2005; Full Text ")
	message = paste(message,'<div><span class=\'emphasis\'>', prob, '%</span> probability of cancer. For next step, consider ');
	if (prob >= 5){
		message = paste(message, '<style>.emphasis{color:red;font-weight:bold}</style>');
		};
	#Messages based on probability
	if (prob > 65){
		message = paste(message,'both:</div><ul>');
		message = paste(message,'<li><span class=\'emphasis\'>PET scan</span> as probability is high (ACCP 2012 rec 4.2.4.1)');
		message = paste(message,'<li><span class=\'emphasis\'>Surgical diagnosis</span> as probability is high (ACCP 2012 rec 4.6.3.1.1)');
		if (geo_region == "KS"){message = paste(message,'<ul><li><a href=\'http://www.mssconline.org/physician_finder.php?hdnScrollLeft=hdnScrollLeft&amp;hdnScrollTop=0&amp;rowcount=100&amp;ddlspecialty=Thoracic+Surgery\'>List of thoracic surgeons in Wichita region</a> (from the Medical Society of Sedgwick County)</li></ul>');}
		message = paste(message,'</li>');
		};
	if (prob >= 5 && prob <= 65){
		message = paste(message,'</div><ul>');
		message = paste(message,'<li><span class=\'emphasis\'>PET scan</span> as probability is low to moderate (ACCP 2012 rec 4.2.4.1)');
		if (geo_region == "KS"){message = paste(message,'<ul><li>If negative, consider bronchoscopic diagnosis. <a href=\'http://www.mssconline.org/physician_finder.php?hdnScrollLeft=hdnScrollLeft&amp;hdnScrollTop=0&amp;rowcount=100&amp;ddlspecialty=Pulmonary+Disease\'>List of pulmonologists in Wichita region</a>.</li><li>If positive, consider surgical diagnosis. <a href=\'http://www.mssconline.org/physician_finder.php?hdnScrollLeft=hdnScrollLeft&amp;hdnScrollTop=0&amp;rowcount=100&amp;ddlspecialty=Thoracic+Surgery\'>List of thoracic surgeons in Wichita region</a>.</li></ul>');}
		message = paste(message,'</li>');
		};
	if (prob < 5){
		message = paste(message,'</div><ul>');
		message = paste(message,'<li><span class=\'emphasis\'>CT surveillance</span> as probability is very low (ACCP 2012 rec 4.5.1.1)');
		if (geo_region == "KS"){message = paste(message,'<ul><li>Consider pulmonary consultation. <a href=\'http://www.mssconline.org/physician_finder.php?hdnScrollLeft=hdnScrollLeft&amp;hdnScrollTop=0&amp;rowcount=100&amp;ddlspecialty=Pulmonary+Disease\'>List of pulmonologists in Wichita region</a>.</li></ul>');}
		message = paste(message,'</li>');
		};
	if (diameter <= 8){
		message = paste(message,'<li>Since diameter =< 8 mm, see section <a href=\'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3749714/#__sec30title\'>5.0 Solid Nodules Measuring ≤ 8 mm in Diameter</a> of ACCP guidelines for more specific suggestions</li>');
		}
	#Finish assembling message
	message = paste(message, '</ul>');
	message = paste(message, '<div>');
	if (geo_region == "KS"){message = paste(message,'Regional guidance adapted from national guidelines by ... . ');}
	message = paste(message, 'Additional detail in guidelines referenced below.</div>');
  list(message = message)
#Swensen SJ, Silverstein MD, Ilstrup DM, Schleck CD, Edell ES: The probability of malignancy in solitary pulmonary nodules. Application to small radiologically indeterminate nodules. Arch Intern Med 157. (8): 849-855.1997;
# Herder GJ, van Tinteren H, Golding RP, et al: Clinical prediction model to characterize pulmonary nodules: validation and added value of 18F-fluorodeoxyglucose positron emission tomography. Chest 128. (4): 2490-2496.2005; Full Text 
}
