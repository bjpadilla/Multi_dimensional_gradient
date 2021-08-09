#########################################################
#########################################################
######## Padilla and Sutherland - PLOS One ##############
######## American Robin Case Study Analysis #############
#########################################################

# Load Libraries
library(unmarked)
library(AICcmodavg)
library(raster)

# Load Data
load("umf_pieces.Rdata") 

# create unmarked frame
occframe <- unmarkedFrameOccu(y = ymat, siteCovs = sitedata)
str(occframe)

# Fit Detection Models
m1 <- occu(~ST+HS+BG+date+datQ ~ ST*HS*BG,data=occframe)
m2 <- occu(~ST*HS+BG+date+datQ ~ ST*HS*BG,data=occframe)
m3 <- occu(~ST*BG+HS+date+datQ ~ ST*HS*BG,data=occframe)
m4 <- occu(~ST*(date+datQ)+BG+HS ~ ST*(HS*BG),data=occframe)
m5 <- occu(~ST+HS+BG+date ~ ST*HS*BG,data=occframe)
m6 <- occu(~ST*HS+BG+date ~ ST*HS*BG,data=occframe)
m7 <- occu(~ST*BG+HS+date ~ ST*HS*BG,data=occframe)
m8 <- occu(~ST*date+BG+HS ~ ST*HS*BG,data=occframe)
m9 <- occu(~ST+HS+BG ~ ST*HS*BG,data=occframe)
m10 <- occu(~ST+HS ~ ST*HS*BG,data=occframe)
m11 <- occu(~ST+BG ~ ST*HS*BG,data=occframe)
m12 <- occu(~ST*(HS+BG) ~ ST*HS*BG,data=occframe)
m13 <- occu(~ST*HS ~ ST*HS*BG,data=occframe)
m14 <- occu(~ST*BG ~ ST*HS*BG,data=occframe)
m15 <- occu(~date ~ ST*HS*BG,data=occframe)
m16 <- occu(~ST*date+HS ~ ST*HS*BG,data=occframe)
m17 <- occu(~ST*date+BG ~ ST*HS*BG,data=occframe)
m18 <- occu(~ST+date+datQ ~ ST*HS*BG,data=occframe)
m19 <- occu(~ST*(date+datQ) ~ ST*HS*BG,data=occframe)
m20 <- occu(~ST+date ~ ST*HS*BG,data=occframe)
m21 <- occu(~ST*date ~ ST*HS*BG,data=occframe)
m22 <- occu(~1 ~ ST*HS*BG,data=occframe)
m23 <- occu(~BG ~ ST*HS*BG,data=occframe)
m24 <- occu(~HS ~ ST*HS*BG,data=occframe)
m25 <- occu(~date+datQ ~ ST*HS*BG,data=occframe)
m26 <- occu(~ST ~ ST*HS*BG,data=occframe)
detmods <- list("S+H+B+D+Dq"  = m1,
                "S*H+B+D+Dq"  = m2,
                "S*B+H+D+Dq"  = m3,
                "S*(D+Dq)+H+B"= m4,
                "S+H+B+D"     = m5,
                "S*H+B+D"     = m6,
                "S*B+H+D"     = m7,
                "S*D+H+B"     = m8,
                "S+H+B"       = m9,
                "S+H"         = m10,
                "S+B"         = m11,
                "S*(H+B)"     = m12,
                "S*H"         = m13,
                "S*B"         = m14,
                "D"           = m15,
                "S*D+H"       = m16,
                "S*D+B"       = m17,
                "S+D+Dq"      = m18,
                "S*(D+Dq)"    = m19,
                "S+D"         = m20,
                "S*D"         = m21,
                "null"        = m22,
                "B"           = m23,
                "H"           = m24,
                "D+Dq"        = m26,
                "S"           = m25)
aictab(detmods)

# Fit Occupancy Models
occmods <- list(m1 = occu(~ST*date+datQ+HS+BG ~ST * (HS*BG),data=occframe),
             m2 = occu(~ST*date+datQ+BG+HS ~ST * (HS+BG),data=occframe),
             m3 = occu(~ST*date+datQ+BG+HS ~ST+(HS*BG),data=occframe),
             m4 = occu(~ST*date+datQ+BG+HS ~ST+(HS+BG),data=occframe),
             m5 = occu(~ST*date+datQ+BG+HS ~ST*HS+BG,data=occframe),
             m6 = occu(~ST*date+datQ+BG+HS~ST*BG+HS,data=occframe),
             m7 = occu(~ST*date+datQ+BG+HS ~HS+BG,data=occframe),
             m8 = occu(~ST*date+datQ+BG+HS ~HS*BG,data=occframe),
             m9 = occu(~ST*date+datQ+BG+HS ~ST+HS,data=occframe),
             m10 = occu(~ST*date+datQ+BG+HS ~ST+BG,data=occframe),
             m11 = occu(~ST*date+datQ+BG+HS ~ST*HS,data=occframe),
             m12 = occu(~ST*date+datQ+BG+HS ~ST*BG,data=occframe),
             m13 = occu(~ST*date+datQ+BG+HS ~HS,data=occframe),
             m14 = occu(~ST*date+datQ+BG+HS ~BG,data=occframe),
             m15 = occu(~ST*date+datQ+BG+HS ~ST, data=occframe),
             m16 <- occu(~ST*date+datQ+BG+HS ~1, data=occframe))
aictab(occmods)

