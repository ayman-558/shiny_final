

ds <- heart
# add names to dataset
names(ds) <- c( "age", "sex", "cp",
                "trestbps", "chol",
                "fbs", "restecg",
                "thalach","exang",
                "oldpeak","slope",
                "ca","thal","target")


feature.list <- list("age" = "age", "sex" ="sex",
                     "cp"= "cp","trestbps" = "trestbps",
                     "chol"="chol","fbs"="fbs",
                     "restecg"="restecg","thalach"="thalach",
                     "exang"="exang","oldpeak"="oldpeak",
                     "slope"="slope","ca"="ca","thal"="thal")

# keep an original copy of the data to retain classes
# for displays
original.ds <- ds
ds <- na.omit(ds)
# change the class of all columns to numeric
ds <- as.data.frame(apply(ds, 2, as.numeric))

# remove na/missing values (original data shows as ?)

# all values of num > 0 are cases of heart disease 
# as per the data descriptions at the uci repository
ds$target <- factor(ds$target, levels = c(0,1), labels = c("negative", "positive"))

# standardize/normalize the data
# for knn we want to ensure the distance is normalized for all 
# features

# standardize all point except the response variable
standardized.X <- scale(ds[,-14])
set.seed(55)

# create training and test sets
training.index <- createDataPartition(ds$target, p = .8,list = F)
train.X <- standardized.X[training.index,]
test.X  <- standardized.X[-training.index,]
train.Y <- ds$target[training.index]
test.Y <- ds$target[-training.index]

# table settings ####

table.settings <- list(searching = F, pageLength = 5, bLengthChange = F,
                       bPaginate = F, bInfo = F )


# define theme for ggplots ####
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]
  text.size <- 14
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=text.size,color=color.axis.title)) +
    theme(legend.title = element_text(size=text.size,color=color.axis.title)) +
    theme(legend.position = "bottom") +
    theme(legend.direction = "vertical") +
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=text.size, vjust=1.25)) +
    theme(axis.text.x=element_text(size=text.size,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=text.size,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=text.size,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=text.size,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

