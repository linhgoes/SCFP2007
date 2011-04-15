rm(list=ls())

# Code used to recreate analysis goes here
# Exploratory commands go in the console

consf <- read.table("scfp2007_data.csv", sep="," , header = TRUE)
# codebook <- read.csv("		scfp2007_codebook.csv", header=TRUE)

# Education vs. Debt/Savings
#total financial assets: FIN=LIQ+CDS+NMMF+STOCKS+BOND+RETQLIQ+SAVBND+CASHLI+OTHMA+OTHFIN;
#total debt;
#DEBT=MRTHEL+RESDBT+OTHLOC+CCBAL+INSTALL+ODEBT;
#EDN_INST        ,Total value of education loans held by household
#EDCL            ,Education category of head of household
#    				1=no high school diploma/GED, 2=high school diploma or GED,
#    				3=some college, 4=college degree;
#SAVING          ,Total value of savings accounts held by household

attach(consf)
consf.edudebt <- data.frame(FIN=FIN, SAVING=SAVING, DEBT=DEBT, EDCL=EDCL, EDN_INST=EDN_INST)
detach(consf)

attach(consf.edudebt)
# How many people have zero savings? 
length(which(SAVING==0))

# Histogram depicting how much people are saving (assuming they are saving between (0,3000]), broken into 50 sections
# IQR(SAVING) = 3000
png("SavingsHisto.png")
hist(SAVING[which(SAVING <= 3000 & SAVING > 0)], breaks=50, main="Distribution of savings accounts between (0,3000]",xlab="Account Balance")
dev.off()
detach(consf.edudebt)

zerosavings <- consf.edudebt[which(consf.edudebt$SAVING==0),]
summary(zerosavings$DEBT)

# Does debt grow with income?
consf.cheat <- data.frame(INCOME = consf$INCOME, DEBT = consf$DEBT)
consf.cheat$INCOME[which(consf.cheat$INCOME == 0)] <- 1
consf.cheat$DEBT[which(consf.cheat$DEBT == 0)] <- 1
glm.cheatlog <- glm(log(consf.cheat$DEBT, 10) ~ log(consf.cheat$INCOME, 10))

png("logDebtVsIncome.png")
plot(log(consf.cheat$INCOME, 10),log(consf.cheat$DEBT, 10), main="Debt and Income Ratio (corrected)",xlab="log(INCOME)", ylab="log(DEBT)")
abline(glm.cheatlog)
dev.off()

# It appears that people with no income have significant debt. So maybe a better question is "Does debt grow with networth?"

consf.networth <- data.frame(DEBT = consf$DEBT, NETWORTH = consf$NETWORTH)
consf.networth$NETWORTH[which(consf.networth$NETWORTH <= 0)] <- 1
consf.networth$DEBT[which(consf.networth$DEBT == 0)] <- 1
glm.networthlog <- glm(log(consf.networth$DEBT, 10) ~ log(consf.networth$NETWORTH, 10))

png("logDebtVsNetworth.png")
plot(log(consf.networth$NETWORTH, 10),log(consf.networth$DEBT, 10), main="Debt and Networth Ratio (corrected)",xlab="log(Networth)", ylab="log(DEBT)")
abline(glm.networthlog)
dev.off()

#What is the average networth of people who do not work?

consf.work <- data.frame(OCCAT2 = consf$OCCAT2, NETWORTH = consf$NETWORTH)
consf.work<-consf.work[which(consf.work$OCCAT2 == 4),]
summary(consf.work$NETWORTH)

# What is the average debt/savings ratio for households of varying educational
# backgrounds?

dropouts <- consf.edudebt[which(consf.edudebt$EDCL==1),]
hs.grads <- consf.edudebt[which(consf.edudebt$EDCL==2),]
some.college <- consf.edudebt[which(consf.edudebt$EDCL==3),]
college.grads <- consf.edudebt[which(consf.edudebt$EDCL==4),


summary(dropouts)
summary(college.grads)

hist(dropouts$DEBT[which(dropouts$DEBT < IQR(dropouts$DEBT))])

#Is there a relationship between the types of information sources for borrowing and household debt?  Are certain sources more reliable?
glm.bcall <- glm(consf$DEBT ~ consf$BCALL)
summary(glm.bcall)
glm.bdont <- glm(consf$DEBT ~ consf$BDONT)
summary(glm.bdont)
glm.bfinplan <- glm(consf$DEBT ~ consf$BFINPLAN)
summary(glm.bfinplan)
glm.bfinpro <- glm(consf$DEBT ~ consf$BFINPRO)
summary(glm.bfinpro)
glm.binternet <- glm(consf$DEBT ~ consf$BINTERNET)
summary(glm.binternet)
glm.bmag <- glm(consf$DEBT ~ consf$BMAGZNEWS)
summary(glm.bmag)
glm.bmail <- glm(consf$DEBT ~ consf$BMAILADTV)
summary(glm.bmail)
glm.bother <- glm(consf$DEBT ~ consf$BOTHER)
summary(glm.bother)
glm.self <- glm(consf$DEBT ~ consf$BSELF)
summary(glm.self)

# Is there a relationship between debt and family structure?
consf.fam <- 

#Extra code

#consf.networth <- consf.networth[which(consf.networth$NETWORTH <= 0),]
#glm.networthlog <- glm((consf.networth$DEBT) ~ (consf.networth$NETWORTH))


# Variables we want:
# AGE             ,Age of head of household
#BCALL           ,Information used for borrowing decisions: call around
#BDONT           ,Information used for borrowing decisions: never borrow
#BFINPLAN        ,"Information used for borrowing decisions: lawyer, accountant, financial planner"
#BFINPRO         ,"Information used for borrowing decisions: banker, broker, real estate broker, builder, dealer, insurance agent"
#BFRIENDWORK     ,"Information used for borrowing decisions: friends, material from work/business contacts"
#BINTERNET       ,Information used for borrowing decisions: internet/online service
#BMAGZNEWS       ,"Information used for borrowing decisions: magazines, newspapers, books"
#BMAILADTV       ,"Information used for borrowing decisions: material in mail, tv, radio, advertisements, telemarketer"
#BOTHER          ,Information used for borrowing decisions: other sources
#BSELF           ,"Information used for borrowing decisions: self, shop around, other personal research"

#RACE            ,Race/ethnicity of respondent

#SAVRES1         ,Reason for saving: can't save
#SAVRES2         ,Reason for saving: education
#SAVRES3         ,Reason for saving: family
#SAVRES4         ,Reason for saving: home
#SAVRES5         ,Reason for saving: purchases
#SAVRES6         ,Reason for saving: retirement
#SAVRES7         ,Reason for saving: liquidity/the future
#SAVRES8         ,Reason for saving: investment
#SAVRES9         ,Reason for saving: no particular reason