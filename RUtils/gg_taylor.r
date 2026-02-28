#==========================================================================================
#==========================================================================================
#     Function gg_taylor.  This is similar to the taylor.diagram (package plotrix), but
# using the tidyverse language.
#
# INPUT
#
# x              -- Data set (data.frame, data.table, or tibble, it will be coerced to
#                      tibble.
#
# obser          -- Column(s) in x that represents the observations. Default is a single
#                      column named "obs".  Each column may contain multiple variables or
#                      sites, which are described by variable "group". If multiple columns
#                      are provided, each of them may be displayed in a different panel (if
#                      variable panel is set), or multiple colours will be used to
#                      distinguish. It may be necessary to rearrange the data, check
#                      functions melt, pivot_longer and pivot_wider, which may help.
#                      **IMPORTANT**: If multiple "obser" columns are provided, there can
#                      only be a single "model" column.
#
# model          -- Column(s) in x that represents the model results.  Default is a single
#                      column named "mod".  This may contain multiple quantities, which can
#                      be accounted by variables "group" and "panel".  For example, it is
#                      possible to display multiple variables, realisations, seasons, etc.
#                      at the same time. It may be necessary to rearrange the data, check
#                      functions melt and pivot_longer which may help.
#                      **IMPORTANT**: If multiple "model" columns are provided, there can
#                      only be a single "obser" column.
#
# group          -- Column in x that describes how to classify the data.  Each group will
#                      have a specific colour in the diagram.  If none is provided, a
#                      single point will be pooled.
#
# panel          -- Column in x that describes how to split the data.  Each panel will be
#                      plot in a separate sub-figure in the diagram. If none is provided,
#                      a single panel will be plot.
#
# multi_opts     -- List of options for handling data annotation (either from models or
# group_opts           from observations) that has multiple sources. This is used only if
#                      more than one source exists for either "obser" or "model".
# + title        .. Title for caption
# + label        .. Labels for model. This must have the same length as variable model.
#                      If label is not provided, the code will use the same category
#                      names as variable model. For "multi" at all cases, and for "group"
#                      when the variable is a character, we strongly recomend to use a
#                      named vector in which the names match the values in the group
#                      column (e.g. `label = c(class_a = "A", class_b = "B")`
#
# colour_opts    -- List of options for colours.  The following elements can be provided.
# + by           .. In case multiple models and groups are provided, use colour_by
#                      to indicate which dimension should be distinguished by colour.
#                      Options are "multi" or "group".  The other one will be
#                      distinguished by shape.  In case only one of them have multiple
#                      categories, we use both shape and colours to distinguish the
#                      categories.
# + level        .. Colours for the categories distinguished by shapes. If NULL, the
#                      colours will be automatically assigned.  Otherwise, this can be a
#                      palette from package RColorBrewer, a palette in package viridis, a
#                      function that takes the number of sought colours as the first
#                      argument, or a vector with as many colours as categories. It
#                      providing specific colours for categories that are characters, we
#                      strongly recomend to use a named vector in which the names match
#                      the values in the group column
#                      (e.g. `label = c(class_a = "slateblue", class_b = "darkorange")`
# + reverse      .. In case colour is a palette or function, reverse it?
#
# shape_opts     -- List of options for shapes.  The following elements can be provided.
# + level        .. List of shapes for the categories distinguished by shapes.  It can be
#                      a vector of numbers (same as those defined by lty) or characters
#                      (similar to ggplot), with as many elements as categories.
#                      If NULL, the shapes will be automatically assigned. If providing
#                      specific shapes for categories that are character, we strongly
#                      recomend to use a named vector in which the names match the values
#                      in the group column (e.g. `label = c(class_a = 15L, class_b = 6L)`
#                      IMPORTANT: avoid using more than 6 shapes.
# + solid        .. In case shape is NULL, use solid shapes (TRUE or FALSE).
# + size         .. Size for symbols.
# + stroke       .. Stroke width for symbols.
#
# panel_opts     -- List with options for panels.  The following elements can be provided:
#  + label       .. Labels, to appear in the sub-plot titles (used only if more than one
#                      panel exists). This must match the number of unique groups.  If
#                      none is provided, the code will use the unique values in variable
#                      panel.  If variable group is a character, we strongly recomend to
#                      use a named vector in which the names match the values in the panel
#                      column (e.g. label = c(class_a = "A", class_b = "B")
#  + tag_type    .. Type of annotation tags. This follows the same idea of
#                      patchwork::plot_annotation. Options are "a" for lowercase
#                      letters, "A" for uppercase letters, "1" for numbers, "i" for
#                      lowercase Roman numerals, "I" for uppercase Roman numerals.  Empty
#                      character will skip levels.
# + prefix       .. String to appear before the tag.
# + suffix       .. String to appear after the tag and before the panel labels.
# + sep          .. Separator between tag, prefix, and suffix. There will be always a
#                      space between tag and labels if panel_levels is not empty.
#
# axis_opts      -- List of options for the main axes.
# + colour       .. Colour for main axes
# + linetype     .. Line type for main axes
# + size         .. Line width for main axes
#
# sigma_opts, corr_opts, gamma_opts
#                -- List of options for the standard deviation of the model (sigma),
#                      correlation between model and observations (corr), and standard
#                      deviation of residuals (gamma) "axes".
# + name         .. Name for this "axis".
# + at           .. Labels for this "axis". If NULL this will be automatically defined.
# + colour       .. Colour for this axis.
# + linetype     .. Line type for this axis.
# + size         .. Line width for this axis.
# + family       .. Font family for this axis and labels.
# + fontsize     .. Font size for this axis and labels.
#
# ideal_opts     -- List of options for plotting the ideal point.
# + colour       .. Colour for ideal point
# + fill         .. Background colour for ideal point
# + starshape    .. Shape for the ideal point (based on geom_star)
# + size         .. Shape size
# + starstroke   .. Thickness of the symbol line
#
# main_title     -- General title for the entire figure (single title even for multiple
#                      panels).
#
# subtitle       -- General subtitle for the entire figure (single title even for multiple
#                      panels).
#
# extra_legend   -- Add extra plot space for legend: TRUE means yes, FALSE (default) means
#                      no.
#
# f_legend       -- Fraction of the plot area for displaying legends. It must be between
#                   0 and 1.
#
# base_size      -- Size for theme in pt
#
# base_family    -- Family type for theme
#
# force_pos      -- Force positive correlation only? Default is FALSE
#
#
# OUTPUT
#
# A "patchwork" structure (compatible with ggplot).
#------------------------------------------------------------------------------------------
gg_taylor = function( x
                    , obser          = "obser"
                    , model          = "model"
                    , group          = character(0L)
                    , panel          = character(0L)
                    , multi_opts     = list()
                    , group_opts     = list()
                    , colour_opts    = list()
                    , shape_opts     = list()
                    , panel_opts     = list()
                    , axis_opts      = list()
                    , sigma_opts     = list()
                    , gamma_opts     = list()
                    , corr_opts      = list()
                    , ideal_opts     = list()
                    , main_title     = "Taylor Diagram"
                    , subtitle       = patchwork::waiver()
                    , extra_legend   = FALSE
                    , f_legend       = 0.3
                    , base_size      = 12
                    , base_family    = if(Sys.info()["sysname"] %in% "Darwin"){"Helvetica"}else{"Nimbus Sans L"}
                    , force_pos      = c("default","always","never")
                    ){ #end gg_taylor


   #--- Make sure force_pos is valid
   if (is.logical(force_pos)){
      force_pos = ifelse( test = force_pos[1L], yes = "always", no = "default")
   }else{
      force_pos = match.arg(force_pos,several.ok=FALSE)
   }#end if (is.logical(force_pos))
   #---~---


   #--- Factor to convert pt to mm
   pt_2_mm = 0.352778
   #---~---

   #--- List of palettes in package RColorBrewer and viridis
   brewer_pal_info  = rownames(RColorBrewer::brewer.pal.info)
   viridis_pal_info = as.character(lsf.str("package:viridis"))
   viridis_pal_info = viridis_pal_info[! grepl(pattern="^scale_",x=viridis_pal_info)]
   #---~---


   #--- Coerce data to a tibble object.
   x = as_tibble(x)
   #---~---


   #--- Save input observations and model with prefix in_ to avoid ambiguity.
   in_obser = obser
   in_model = model
   in_group = group
   in_panel = panel
   #---~---

   #---~---
   #   Find the length of the input values
   #---~---
   cnt_obser   = length(in_obser)
   cnt_model   = length(in_model)
   cnt_group   = length(in_group)
   cnt_panel   = length(in_panel)
   multi_obser = cnt_obser > 1L
   multi_model = cnt_model > 1L
   #---~---


   #---~---
   #   Stop if the user set both models and observations to have multiple columns
   #---~---
   if (multi_obser & multi_model){
      cat0("---~---")
      cat0("   FATAL ERROR! ")
      cat0("---~---")
      cat0(" ")
      cat0("   Number of observation columns -- ",cnt_obser,".")
      cat0("   Number of model columns       -- ",cnt_model,".")
      cat0(" ")
      cat0("---~---")
      stop(" Taylor plots cannot have multiple columns for both \"obser\" and \"model\"!")
   }#end if (multi_obser & multi_model)
   #---~---



   #---~---
   # Set default options.  They will be superseded by the arguments.
   #---~---
   #--- Model legend.
   multi_opts_def  = list( title = ""
                         , names = if(multi_obser){in_obser}else{in_model}
                         , label = if(multi_obser){in_obser}else{in_model}
                         , parse = FALSE
                         )#end list
   #--- Group legend.
   group_opts_def  = list( title = ""
                         , group = NULL
                         , parse = FALSE
                         )#end list
   #--- Colours.
   colour_opts_def = list( by      = "multi"
                         , names   = NULL
                         , label   = NULL
                         , levels  = NULL
                         , reverse = FALSE
                         , parse   = FALSE
                         )#end list
   #--- Shape.
   shape_opts_def  = list( names   = NULL
                         , levels  = NULL
                         , label   = NULL
                         , solid   = TRUE
                         , size    = 3
                         , stroke  = 1
                         , parse   = FALSE
                         )#end list
   #--- Panel.
   panel_opts_def  = list( names    = NULL
                         , label    = NULL
                         , tag_type = "a"
                         , prefix   = "("
                         , suffix   = ")"
                         , sep      = ""
                         )#end list
   #--- Axes
   axis_opts_def   = list( colour    = par("fg")
                         , linetype  = "solid"
                         , size      = 0.6
                         , fontsize  = 0.9 * base_size
                         )#end list
   #--- Sigma (model/obs) curves
   sigma_opts_def  = list( name     = parse(text="sigma[M*o*d*e*l]*phantom(1)*sigma[R*e*f*e*r*e*n*c*e]^{-1}")
                         , at       = NULL
                         , colour   = "#262626"
                         , linetype = "solid"
                         , size     = 0.3
                         , family   = base_family
                         , fontsize = 0.9 * base_size * pt_2_mm
                         )#end list
   #--- Sigma of residuals (gamma) curves.
   gamma_opts_def  = list( name     = parse(text="sigma[R*e*s*i*d*u*a*l]*phantom(1)*sigma[R*e*f*e*r*e*n*c*e]^{-1}")
                         , at       = NULL
                         , colour   = "#5996B2"
                         , linetype = "e363"
                         , size     = 0.3
                         , family   = base_family
                         , fontsize = 0.9 * base_size * pt_2_mm
                         )#end list
   #--- Correlation curves.
   corr_opts_def   = list( name      = parse(text="rho(x[M*o*d*e*l],x[R*e*f*e*r*e*n*c*e])")
                         , at        = c(0, .2, .4, .6, .7, .8, .9, .95, .99)
                         , colour    = "#686868"
                         , linetype  = "22"
                         , size      = 0.3
                         , family    = base_family
                         , fontsize  = 0.9 * base_size * pt_2_mm
                         )#end list
   #--- Ideal point.
   ideal_opts_def  = list( colour     = "#1E1E1E"
                         , fill       = "#CECECE"
                         , starshape  = 14L
                         , size       = 5
                         , starstroke = 1.25
                         )#end list
   #---~---


   #--- Update lists based on arguments.
   multi_opts  = modifyList( x = multi_opts_def , val = multi_opts  )
   group_opts  = modifyList( x = group_opts_def , val = group_opts  )
   colour_opts = modifyList( x = colour_opts_def, val = colour_opts )
   shape_opts  = modifyList( x = shape_opts_def , val = shape_opts  )
   panel_opts  = modifyList( x = panel_opts_def , val = panel_opts  )
   axis_opts   = modifyList( x = axis_opts_def  , val = axis_opts   )
   sigma_opts  = modifyList( x = sigma_opts_def , val = sigma_opts  )
   gamma_opts  = modifyList( x = gamma_opts_def , val = gamma_opts  )
   corr_opts   = modifyList( x = corr_opts_def  , val = corr_opts   )
   ideal_opts  = modifyList( x = ideal_opts_def , val = ideal_opts  )
   #---~---






   #--- Match arguments for variables with options.
   panel_opts$tag_type  = match.arg( arg        = panel_opts$tag_type
                                   , choices    = c("","a","A","1","i","I")
                                   , several.ok = FALSE
                                   )#end match.arg
   colour_opts$by       = match.arg( arg        = colour_opts$by
                                   , choices    = c("multi","group")
                                   , several.ok = FALSE
                                   )#end match.arg
   #---~---


   #--- Save previous PAR settings.
   par_all  = par(no.readonly=FALSE)
   #---~---


   #--- Make sure all observation variables exist in the input data.
   if (cnt_obser == 0L){
      cat0(" - Variable \"obser\" has length ",cnt_obser,".")
      stop(" Variable \"obser\" must be a character vector with at least 1 element.")
   }else if (! all(in_obser %in% names(x))){
      cat0(" - \"obser\" (",in_obser,") must match a column in the input data.")
      stop(" In gg_taylor: Variable \"obser\" not found in input data \"x\".")
   }#end if (in_obser %in% names(x))
   #---~---


   #--- Make sure the group variable is properly set.
   if (cnt_group == 0L){
      #--- Group was not provided, create a dummy group
      x     = x %>% mutate( group = rep(x=1L,times=nrow(x)) )
      #---~---
   }else if (cnt_group > 1){
      stop(" In gg_taylor: \"group\" cannot have more than 1 element.")
   }else if (! all(in_group %in% names(x))){
      stop(" In gg_taylor: Some columns in \"group\" do not exist in data set \"x\".")
   }#end if (cnt_group == 0)
   #---~---


   #--- Make sure the group variable is properly set.
   if (cnt_panel == 0){
      #--- Group was not provided, create a dummy group
      x     = x %>% mutate( panel = rep(x=1L,times=nrow(x)) )
      #---~---
   }else if (cnt_panel > 1){
      stop(" In gg_taylor: \"panel\" cannot have more than 1 element.")
   }else if (! all(panel %in% names(x))){
      stop(" In gg_taylor: Some columns in \"panel\" do not exist in data set \"x\".")
   }#end if (cnt_panel == 0)
   #---~---


   #--- Make sure the model variable is properly set.
   if (cnt_model == 0){
      cat0(" - Variable \"model\" has length ",cnt_model,".")
      stop(" Variable \"model\" must be a character vector with at least 1 element.")
   }else if (! all(in_model %in% names(x))){
      stop(" In gg_taylor: Some columns \"model\" do not exist in data set \"x\".")
   }#end if (length(in_model) == 0)
   #---~---


   #--- Make sure all labels and label names are defined.
   if (is.null(multi_opts$label)) multi_opts$label = sort(unique(x$multi))
   if (is.null(group_opts$label)) group_opts$label = sort(unique(x$group))
   if (is.null(panel_opts$label)) panel_opts$label = sort(unique(x$panel))
   if (is.null(names(multi_opts$label))){
      multi_opts$names = multi_opts$label
   }else{
      multi_opts$names = names(multi_opts$label)
      names(multi_opts$label) = NULL
   }#end if (is.null(names(multi_opts$label)))
   if (cnt_group > 0L){
      if (is.null(names(group_opts$label))){
         group_opts$names = group_opts$label
      }else{
         group_opts$names = names(group_opts$label)
         names(group_opts$label) = NULL
      }#end if (is.null(names(group_opts$label)))
   }#end if (cnt_group > 0L)
   if (cnt_panel > 0L){
      if (is.null(names(panel_opts$label))){
         panel_opts$names = panel_opts$label
      }else{
         panel_opts$names = names(panel_opts$label)
         names(panel_opts$label) = NULL
      }#end if (is.null(names(panel_opts$label)))
   }#end if (cnt_panel > 0L)
   #---~---


   #---~---
   #   Retrieve the names from labels in the right order
   #---~---
   multi_names        = multi_opts$names
   group_names        = group_opts$names
   panel_names        = panel_opts$names
   multi_label        = multi_opts$label
   group_label        = group_opts$label
   panel_label        = panel_opts$label
   #---~---


   #---~---
   #   Here we may want to split the values differently.
   #---~---
   if (multi_model){
      #--- Rename column with observations to be "obser".
      x = as_tibble(x) %>% rename( obser = in_obser)
      #---~---

      #--- Melt model data, it will help to make the plot.
      x = x %>%
         pivot_longer(cols=all_of(in_model),names_to="multi",values_to="model") %>%
         mutate(multi = match(multi,in_model))
      #---~---
   }else{
      #--- Rename column with observations to be "model".
      x = as_tibble(x) %>% rename( model = in_model)
      #---~---

      #--- Melt observation data, it will help to make the plot.
      x = x %>%
         pivot_longer(cols=all_of(in_obser),names_to="multi",values_to="obser") %>%
         mutate(multi = match(multi,in_obser))
      #---~---
   }#end if (multi_model)
   #---~---


   #---~---
   #   Use the order of variables in the label so colours match legend.
   #---~---
   #   Groups
   if (cnt_group > 0L){
      group_lookup = c( group = in_group )
      x            = x                                  %>%
         rename( all_of(group_lookup) )                 %>%
         mutate( group = match(group,group_opts$names) )
      if (any(is.na(x$group))){
         stop( paste0( " Names in \"group_opts$label\" must match unique values"
                     , " in column \"",group,"\"." ) )
      }#end if (any(is.na(x$group)))
   }#end if (cnt_group > 0L)
   #   Panels
   if (cnt_panel > 0L){
      panel_lookup = c( panel = in_panel )
      x            = x                                  %>%
         rename( all_of(panel_lookup) )                 %>%
         mutate( panel = match(panel,panel_opts$names) )
      if (any(is.na(x$panel))){
         stop( paste0( " Names in \"panel_opts$label\" must match unique values"
                     , " in column \"",panel,"\"." ) )
      }#end if (any(is.na(x$panel)))
   }#end if (cnt_group > 0L)
   #---~---



   #---~---
   #   Make sure group, panel, and reference source are all factorial variables.
   # We do not assign labels or levels because some of them may be dummy, but we
   # made sure all settings are aligned.
   #---~---
   x = x %>%
      mutate( multi = factor(x=multi)
            , group = factor(x=group)
            , panel = factor(x=panel)
            )#end mutate
   #---~---


   #--- Combine data into a single tibble
   xsumm = as_tibble(x) %>%
      group_by(panel,group,multi)                                        %>%
      summarise( sd_model = sd(model,na.rm=TRUE) / sd(obser,na.rm=TRUE)
               , corr     = cor( x      = obser
                               , y      = model
                               , use    = "pairwise.complete.obs"
                               , method = "pearson"              ) )     %>%
      ungroup()                                                          %>%
      mutate( theta = acos(corr)
            , x     = sd_model * cos(theta)
            , y     = sd_model * sin(theta) )
   #---~---


   #--- Make sure all labels are defined, even if they are dummy.
   if (is.null(multi_opts$label)) multi_opts$label = levels(xsumm$multi)
   if (is.null(group_opts$label)) group_opts$label = levels(xsumm$group)
   if (is.null(panel_opts$label)) panel_opts$label = levels(xsumm$panel)
   #---~---


   #---~---
   #   Decide how many panels, symbols and plots we will need.  Groups and the "multi"
   # variables define colours and shape (either way, depending on the settings)
   # and panels define the number of sub-plots and sub-titles.
   #---~---
   xsplit  = xsumm %>% group_by(panel) %>% group_split()
   n_panel = length(panel_opts$label)
   #---~---


   #---~---
   #   Set colours for groups and shapes for models.
   #---~---
   n_multi = length(multi_opts$label)
   n_group = length(group_opts$label)
   #---~---



   #---~---
   #   Set panel annotation.
   #---~---
   if ( panel_opts$tag_type %in% ""){
      #--- No keys, use the title
      panel_opts$title = panel_opts$label
      #---~---
   }else{
      #--- Generate keys
      if (panel_opts$tag_type %in% "a"){
         # Lower case letters
         panel_opts$keys   = letters[sequence(n_panel)]
      }else if (panel_opts$tag_type %in% "A"){
         # Upper case letters
         panel_opts$keys   = LETTERS[sequence(n_panel)]
      }else if (panel_opts$tag_type %in% "1"){
         # Numbers
         panel_opts$keys   = as.character(sequence(n_panel))
      }else if (panel_opts$tag_type %in% "i"){
         # Lower case Roman numerals
         panel_opts$keys   = tolower(as.roman(sequence(n_panel)))
      }else if (panel_opts$tag_type %in% "I"){
         # Upper case Roman numerals
         panel_opts$keys   = toupper(as.roman(sequence(n_panel)))
      }#end if ( panel_opts$tag_type %in% "")
      #--- Prepend keys.
      panel_opts$annot = with(panel_opts, paste(prefix,keys,suffix,sep=sep))
      panel_opts$title = with(panel_opts, paste(annot,label))
      #---~---
   }#end if (panel_opts$tag_type %in% "")
   #---~---



   #---~---
   #   Set colours and shapes.  This will depend on how many "multi" values and groups we
   # define.
   #---~---
   vars_set = c("title","label","levels","names","parse")
   if ( n_multi == 1L){
      #--- Single "multi", use colours and shapes for groups.
      aes_colour        = "group"
      aes_shape         = "group"
      if (n_group == 1L){
         colour_opts[vars_set] = NULL
      }else{
         colour_opts    = modifyList(x=colour_opts,val=group_opts[vars_set])
      }#end if (n_group = 1L)
      shape_opts       = modifyList(x=shape_opts,val=colour_opts[vars_set])
      shape_opts$level = NULL
      n_colour          = n_group
      n_shape           = n_group
      #---~---
   }else if (n_group == 1L){
      #--- Single group, use colours and shapes for "multi".
      aes_colour  = "multi"
      aes_shape   = "multi"
      colour_opts = modifyList(x=colour_opts,val=multi_opts [vars_set])
      shape_opts  = modifyList(x=shape_opts ,val=colour_opts[vars_set])
      n_colour    = n_multi
      n_shape     = n_multi
      #---~---
   }else if (colour_opts$by %in% c("multi")){
      #--- Multiple groups and models, use colours for models and shapes for groups.
      aes_colour  = "multi"
      aes_shape   = "group"
      colour_opts = modifyList(x=colour_opts,val=multi_opts [vars_set])
      shape_opts  = modifyList(x=shape_opts ,val=group_opts [vars_set])
      n_colour    = n_multi
      n_shape     = n_group
      #---~---
   }else{
      #--- Multiple groups and "multi", use colours for groups and shapes for models.
      aes_colour  = "group"
      aes_shape   = "multi"
      colour_opts = modifyList(x=colour_opts,val=group_opts [vars_set])
      shape_opts  = modifyList(x=shape_opts ,val=multi_opts [vars_set])
      n_colour    = n_group
      n_shape     = n_multi
      #---~---
   }#end if (n_multi == 1)
   #---~---


   #--- Set default colour_opts$level in case it is NULL
   if (is.null(colour_opts$level)){
      #--- Decide the number of palettes by the number of colours.
      if (n_colour <= 8L){
         colour_opts$level = "Dark2"
      }else if (n_colour <= 12L){
         colour_opts$level = "Paired"
      }else{
         colour_opts$level = "plasma"
         warning( paste0(" Dimension \"",aes_colour,"\" has ",n_colour," categories. "
                        ," We recommend 12 categories or less for colours." ) )
      }#end if
      #---~---
   }#end if (is.null(colour))
   #---~---



   #--- Define colours.  We test how to generate the palette if needed.
   colour_first = colour_opts$level[1]
   if ( colour_first %in% brewer_pal_info ){
      #--- RColorBrewer palette
      colour_opts$level = RColorBrewer::brewer.pal(n=n_colour,name=colour_first)
      if (colour_opts$reverse) colour_opts$level = rev(colour_opts$level)
      #---~---
   }else if( colour_first %in% viridis_pal_info){
      #--- Viridis palette
      colour_opts$level = viridis::viridis_pal( n         = n_colour
                                              , option    = colour_opts$first
                                              )#end viridis::viridis_pal
      if (colour_opts$reverse) colour_opts$level = rev(colour_opts$level)
      #---~---
   }else if ( ! ("try-error" %in% is(try(match.fun(colour_first),silent=TRUE)) )){
      #--- Other palette function
      colour_fun        = match.fun(colour_first)
      colour_opts$level = colour_fun(n=n_colour)
      if (colour_opts$reverse) colour_opts$level = rev(colour_opts$level)
      #---~---
   }else{
      # Vector, assume these are the colours already. Make sure there are enough colours.
      if (length(colour_opts$level) < n_colour){
         #--- Not enough colours, stop the function.
         cat0(" Length of vector \"colour_opts$level\":  ",length(colour_opts$level),".")
         cat0(" Dimension to be distinguished by colour: ",aes_colour,".")
         stop(" Vector \"colour_opts$level\" length must match the number of categories.")
         #---~---
      }else if (length(colour_opts$level) > n_colour){
         #--- Too many colours, issue a warning but continue.
         cat0(" Length of vector \"colour_opts$level\": ",length(colour_opts$level),".")
         cat0(" Dimension to be distinguished by colour: ",aes_colour,".")
         warning( paste0(" Vector \"colour_opts$level\" length exceeds the number"
                        ," of categories. Using the first, ",n_colour," colour(s).")
                )#end warning
         colour_opts$level = colour_opts$level[sequence(n_colour)]
         #---~---
      }#end if (length(colour_opts$level) < n_colour)
      #---~---
   }#end if ( colour_first %in% brewer_pal_info )
   #---~---



   #--- Define shapes.  Currently a maximum of 20 shapes works (max. 6 is recommended).
   if (n_shape > 20L){
      #--- Too many categories, stop.
      stop( paste0( " Too many shape categories (",n_shape,"). Maximum is 20, and the"
                  , " maximum recommended is 6.") )
      #---~---
   }else if (is.null(shape_opts$level)){
      #--- Make a list of shapes to use.
      shape_first      = scales::shape_pal(solid=shape_opts$solid)(n=6L)
      shape_other      = sequence(20L)[! sequence(20L) %in% shape_first]
      shape_opts$level = c(shape_first,shape_other)[sequence(n_shape)]
      #---~---
   }else if (length(shape_opts$level) < n_shape){
      #--- Not enough shapes, stop the function.
      cat0(" Length of vector \"shape_opts$level\": ",length(shape_opts$level),".")
      cat0(" Dimension to be distinguished by shape: ",aes_shape,".")
      stop(" Vector \"shape_opts$level\" length must match the number of categories.")
      #---~---
   }else if (length(shape_opts$level) > n_shape){
      #--- Too many shapes, issue a warning but continue.
      cat0(" Length of vector \"shape_opts$level\": ",length(shape_opts$level),".")
      cat0(" Dimension to be distinguished by shape: ",aes_shape,".")
      warning( paste0(" Vector \"shape_opts$level\" length exceeds the number of"
                     ," categories. Using the first, ",n_shape," shape(s).")
             )#end warning
      shape_opts$level = shape_opts$level[sequence(n_shape)]

      #---~---
   }#end if (length(shape_opts$level) < n_shape)
   #--- Issue a warning in case the number of shapes is too long.
   if (n_shape > 6L){
      warning( paste0(" Dimension \"",aes_shape,"\" has ",n_shape," categories. "
                     ," We recommend 6 categories or less for shapes." ) )
   }#end if (n_shape > 6L)
   #---~---



   #---~---
   #   Make sure the colour and shapes are in the right order.
   #---~---
   if (! is.null(names(colour_opts$level))){
      idx                      = match(colour_opts$names,names(colour_opts$level))
      colour_opts$level        = colour_opts$level[idx]
      names(colour_opts$level) = NULL
   }#end if (! is.null(names(colour_opts$level)))
   if (! is.null(names(shape_opts$level))){
      idx                      = match(shape_opts$names,names(shape_opts$level))
      shape_opts$level         = shape_opts$level[idx]
      names(shape_opts$level)  = NULL
   }#end if (! is.null(names(shape_opts$level)))
   #---~---

   #---~---
   #   Parse elements if they should be parsed
   #---~---
   if (multi_opts$parse ){
      names_label              = names(multi_opts$label)
      multi_opts$label         = parse(text = multi_opts$label )
      names(multi_opts$label)  = names_label
   }#end if (multi_opts$parse )
   if (group_opts$parse ){
      names_label              = names(group_opts$label)
      group_opts$label         = parse(text = group_opts$label )
      names(group_opts$label)  = names_label
   }#end if (group_opts$parse )
   if (colour_opts$parse ){
      names_label              = names(colour_opts$label)
      colour_opts$label        = parse(text = colour_opts$label )
      names(colour_opts$label) = names_label
   }#end if (colour_opts$parse )
   if (shape_opts$parse ){
      names_label              = names(shape_opts$label)
      shape_opts$label         = parse(text = shape_opts$label )
      names(shape_opts$label)  = names_label
   }#end if (shape_opts$parse )
   #---~---



   #--- Find limits for plot.
   max_sd = 1.2 * max(c(1,xsumm$sd_model),na.rm=TRUE)
   #---~---



   #---~---
   #      Define some settings differently depending on whether this is a positive-only
   # or full correlation Taylor plot.
   #---~---
   quarter    = seq(from=0,to= 90,by=1) * pi / 180.
   half       = seq(from=0,to=180,by=1) * pi / 180.
   pos_corr   = switch( EXPR = force_pos
                      , default = (! any(xsumm$corr %lt% 0.) )
                      , always  = TRUE
                      , never   = FALSE
                      , stop(paste0(" Invalid option for force_pos (",force_pos,")."))
                      )#end switch
   #---~---



   #--- Set points for curves
   theta_gamma = half
   theta_sdarc = if(pos_corr){quarter}else{half}
   #---~---


   #--- Set plot limits.
   xlim       = c(-max_sd*as.numeric(! pos_corr),max_sd)
   ylim       = c(0.,max_sd)
   xlim_buff  = 1.25 * xlim
   ylim_buff  = 1.25 * ylim
   axis_opts$grid  = tibble( x0 = c(xlim[1],     0.)
                           , y0 = c(ylim[1],ylim[1])
                           , x1 = c(xlim[2],     0.)
                           , y1 = c(ylim[1],ylim[2])
                           )#end tibble
   axis_opts$arc   = tibble( theta = theta_sdarc
                           , r     = rep(max_sd,times=length(theta_sdarc))
                           , x     = r * cos(theta)
                           , y     = r * sin(theta)
                           )#end tibble
   #---~---


   #--- Set tibble for the correlation grid
   if (pos_corr){
      corr_opts$at = corr_opts$at[corr_opts$at != 0]
   }else{
      corr_opts$at = sort(unique(c(-corr_opts$at,0,corr_opts$at)))
   }#end if (pos_corr
   corr_opts$grid  = tibble( at    = acos(corr_opts$at)
                           , theta = round(180. * at / pi,2)
                           , label = ifelse( test = corr_opts$at == 0
                                           , yes  = "0"
                                           , no   = sprintf("%g",round(corr_opts$at,3))
                                           )#end ifelse
                           , x0    = rep(0., times = length(corr_opts$at))
                           , y0    = rep(0., times = length(corr_opts$at))
                           , x1    = max_sd * cos(at)
                           , y1    = max_sd * sin(at)
                           , xlab  = 1.02 * x1
                           , ylab  = 1.02 * y1
                           , angle = ifelse( test = theta > 90
                                           , yes  = theta-180
                                           , no   = theta
                                           )#end ifelse
                           , hjust = ifelse( test = angle > 0.
                                           , yes  = 0
                                           , no   = 1
                                           )#end ifelse
                           )#end tibble
   #---~---

   #--- Define a title to correlation at the top.
   corr_opts$title =
      tibble( theta   = if(pos_corr){45}else{90}
            , rlab    = if(pos_corr){1.30 * max_sd}else{1.2 * max_sd}
            , xlab    = rlab * cos(theta*pi/180.)
            , ylab    = rlab * sin(theta*pi/180.)
            , ynudge  = if(pos_corr){0.}else{0.01 * max_sd}
            , label   = corr_opts$name
            , angle   = if (pos_corr){
                           -90. + 180./pi * atan2( y = par_all$din[2]
                                                 , x = par_all$din[1]*(1-f_legend)
                                                 )#end atan2
                        }else{
                           0.
                        }#end if
            , hjust   = 0.5
            , vjust   = 0.5
            )#end tibble
   #---~---


   #---~---
   #   Function for breaks that is more flexible than the default for identity_trans.
   # This allows modifying other parameters of function labeling::extended
   #---~---
   breaks_flex = function(x,n=n_default,...){
      x = x[is.finite(x)]
      if (length(x) == 0L) return(numeric())

      xlwr = min(x)
      xupr = max(x)
      ans = labeling::extended(dmin=xlwr,dmax=xupr,m=n,...)
      return(ans)
   }#end function breaks_flex
   #---~---



   #--- Set labels for the residual standard deviation grid
   if (is.null(gamma_opts$at)){
      gamma_trans        = scales::identity_trans()
      gamma_trans$breaks = breaks_flex
      gamma_opts$at      = gamma_trans$breaks(n=5,x=ylim,Q=c(1, 5, 2, 2.5, 4))[-1]
   }else{
      gamma_opts$at = gamma_opts$at[gamma_opts$at > 0]
   }#end if (is.null(gamma_opts$at))
   #---~---


   #--- Set tibble for gamma grid
   gamma_opts$grid = tibble( id    = rep(seq_along(gamma_opts$at),each=length(theta_gamma))
                           , theta = rep(theta_gamma,times=length(gamma_opts$at))
                           , r     = rep(gamma_opts$at,each=length(theta_gamma))
                           , x     = cos(theta) * r + 1.0
                           , y     = sin(theta) * r
                           , r2    = x*x + y*y
                           )#end tibble
   gamma_opts$grid = gamma_opts$grid %>%
                     mutate( bye = (r2 > max_sd^2) | ( (x < 0) & (pos_corr) )
                           , x   = ifelse(bye,NA_real_,x)
                           , y   = ifelse(bye,NA_real_,y)
                           )#end mutate
   #---~---



   #--- Create adequate annotation information for sigma residuals (aka gamma).
   if (pos_corr){
      #--- Positive correlations only, use the x axis for annotation.
      gamma_opts$annot = tibble( at    = with(gamma_opts,sort(unique(c(-at,0,at))) + 1)
                               , label = sprintf("%g",abs(at - 1.))
                               )#end tibble
      gamma_opts$annot = gamma_opts$annot %>%
                         filter( (at > 0) & (at < max_sd) )
      #---~---
   }else{
      #--- Positive and negative correlations, plot labels in the plot area.
      keep_median = function(x,bye) x[floor(median(which(! bye)))]
      gamma_opts$annot = gamma_opts$grid %>%
                         group_by(id)    %>%
                         summarise( id    = median(id)
                                  , at    = median(r)
                                  , label = sprintf("%g",at)
                                  , xlab  = keep_median(x=x,bye=bye)
                                  , ylab  = keep_median(x=y,bye=bye) )
      #---~---

      #--- Define a title to gamma at the top.
      gamma_opts$title =
         tibble( theta = round( x      = 90.
                                       + 180./pi * atan2( y = par_all$din[2]
                                                        , x = par_all$din[1]*(1-f_legend)
                                                        )#end atan2
                              , digits = 2
                              )#end round
               , rlab  = 1.3  * max_sd
               , xlab  = rlab * cos(theta*pi/180.)
               , ylab  = rlab * sin(theta*pi/180.)
               , label = gamma_opts$name
               , angle = ( theta + 90 ) %% 180.
               , hjust = 0.5
               , vjust = 0.5
               )#end tibble
      #---~---
   }#end if (pos_corr)
   #---~---



   #--- Set levels for sigma breaks
   if (is.null(sigma_opts$at)){
      #---~---
      #   Default labels depend on whether or not we are plotting the full diagram.
      #---~---
      if (pos_corr){
         #---~---
         #   Positive correlation only, use the default settings.
         #---~---
         sigma_trans        = trans_new( name      = "sigma_trans"
                                       , transform = identity_trans
                                       , inverse   = identity_trans
                                       )#end trans_new
         sigma_trans$breaks = breaks_flex
         sigma_opts$at      = sigma_trans$breaks(n=5,x=xlim,Q=c(1, 5, 2, 2.5, 4))
         #---~---
      }else{
         #---~---
         #   Positive and negative correlations, borrow from gamma.
         #---~---
         sigma_opts$at      = gamma_opts$at[gamma_opts$at != 0]
         #---~---
      }#end
   }else{
      sigma_opts$at      = sigma_opts$at[sigma_opts$at != 0]
   }#end if
   #---~---

   #---~---
   #   Remove out-of-bound values and the dummy values.
   #---~---
   sigma_keep       = 
      with(sigma_opts, ( at >= xlim[1L] ) & ( at <= xlim[2L] ) & ( at != 0 ))
   sigma_opts$at    = sigma_opts$at[sigma_keep]
   sigma_opts$label = sprintf("%g",abs(sigma_opts$at))
   #---~---


   #--- Set tibble for sigma grid
   sigma_opts$grid = tibble( id    = rep(seq_along(sigma_opts$at),each=length(theta_sdarc))
                           , theta = rep(theta_sdarc,times=length(sigma_opts$at))
                           , r     = rep(sigma_opts$at,each=length(theta_sdarc))
                           , x     = cos(theta) * r
                           , y     = sin(theta) * r
                           )#end tibble
   #---~---


   #---~---
   #   Define the sweet spot for predictions (100% correlation, equal variability between
   # observations and models
   #---~---
   ideal = tibble(x=1,y=0)
   #---~---



   #--- Initialise the ggplot
   taylor = replicate(n_panel,list())
   for (n in sequence(n_panel)){
      #--- Initialise ggplot
      gg_now = ggplot()
      gg_now = gg_now + theme_minimal( base_family = base_family
                                     , base_size   = base_size
                                     )#end theme_minimal
      gg_now = gg_now + theme( line        = element_blank()
                             , rect        = element_blank()
                             )#end theme
      #---~---

      #--- Add local title in case more than one panel exists.
      if (n_panel > 1) gg_now = gg_now + labs(title=panel_opts$title[n])
      #---~---



      #--- Set axes
      if (pos_corr){
         gg_now = gg_now + scale_x_continuous( name   = gamma_opts$name
                                             , limits = xlim_buff
                                             , breaks = gamma_opts$annot$at
                                             , labels = gamma_opts$annot$label
                                             , expand = expansion(0,0)
                                             )#end scale_x_continuous
         gg_now = gg_now + scale_y_continuous( name   = sigma_opts$name
                                             , limits = ylim_buff
                                             , breaks = sigma_opts$at
                                             , labels = sigma_opts$label
                                             , expand = expansion(0,0)
                                             )#end scale_x_continuous
      }else{
         gg_now = gg_now + scale_x_continuous( name   = sigma_opts$name
                                             , limits = xlim_buff
                                             , breaks = c( -rev(sigma_opts$at)
                                                         , sigma_opts$at
                                                         )#end c
                                             , labels = c( rev(sigma_opts$label)
                                                         , sigma_opts$label
                                                         )#end c
                                             , expand = expansion(0,0)
                                             )#end scale_x_continuous
         gg_now = gg_now + scale_y_continuous( name   = NULL
                                             , limits = ylim_buff
                                             , breaks = NULL
                                             , labels = NULL
                                             , expand = expansion(0,0)
                                             )#end scale_x_continuous

      }#end if (pos_corr)
      #---~---



      #--- Plot axis line
      gg_now = gg_now + geom_segment( data    = axis_opts$grid
                                    , mapping = aes( x        = x0
                                                   , y        = y0
                                                   , xend     = x1
                                                   , yend     = y1
                                                   )#end aes
                                    , colour   = axis_opts$colour
                                    , linetype = axis_opts$linetype
                                    , size     = axis_opts$size
                                    )#end geom_segment
      gg_now = gg_now + geom_line   ( data    = axis_opts$arc
                                    , mapping = aes( x        = x
                                                   , y        = y
                                                   )#end aes
                                    , colour   = axis_opts$colour
                                    , linetype = axis_opts$linetype
                                    , size     = axis_opts$size
                                    )#end geom_segment
      #---~---



      #--- Plot sigma curves
      gg_now = gg_now + geom_line( data    = sigma_opts$grid
                                 , mapping = aes( x        = x
                                                , y        = y
                                                , group    = id
                                                )#end aes
                                 , colour   = sigma_opts$colour
                                 , linetype = sigma_opts$linetype
                                 , size     = sigma_opts$size
                                 )#end geom_segment
      #---~---



      #--- Plot gamma curves
      gg_now = gg_now + geom_line( data    = gamma_opts$grid
                                 , mapping = aes( x        = x
                                                , y        = y
                                                , group    = id
                                                )#end aes
                                 , colour   = gamma_opts$colour
                                 , linetype = gamma_opts$linetype
                                 , size     = gamma_opts$size
                                 )#end geom_segment
      #---~---



      #--- Plot gamma labels
      if (! pos_corr){
         gg_now = gg_now + geom_label( data         = gamma_opts$annot
                                     , mapping      = aes( x        = xlab
                                                         , y        = ylab
                                                         , label    = label
                                                        )#end aes
                                    , colour        = gamma_opts$colour
                                    , family        = gamma_opts$family
                                    , size          = gamma_opts$fontsize
                                    , hjust         = 0.5
                                    , vjust         = 0.5
                                    , fill          = "white"
                                    , label.padding = unit(0.10,"lines")
                                    , label.r       = unit(0.05,"lines")
                                    , linewidth     = 0.
                                    )#end geom_segment
         gg_now = gg_now + geom_text( data    = gamma_opts$title
                                    , mapping = aes( x        = xlab
                                                   , y        = ylab
                                                   )#end aes
                                    , label    = gamma_opts$title$label
                                    , angle    = gamma_opts$title$angle
                                    , hjust    = gamma_opts$title$hjust
                                    , vjust    = gamma_opts$title$vjust
                                    , parse    = is.expression(gamma_opts$name)
                                    , colour   = gamma_opts$colour
                                    , family   = gamma_opts$family
                                    , size     = gamma_opts$fontsize
                                    )#end geom_segment
      }#end if (! pos_corr)
      #---~---



      #--- Plot the correlation grid
      gg_now = gg_now + geom_segment( data    = corr_opts$grid
                                    , mapping = aes( x        = x0
                                                   , y        = y0
                                                   , xend     = x1
                                                   , yend     = y1
                                                   )#end aes
                                    , colour   = corr_opts$colour
                                    , linetype = corr_opts$linetype
                                    , size     = corr_opts$size
                                    )#end geom_segment
      #---~---



      #--- Plot the correlation labels
      gg_now = gg_now + geom_text( data    = corr_opts$grid
                                 , mapping = aes( x        = xlab
                                                , y        = ylab
                                                , label    = label
                                                , angle    = angle
                                                , hjust    = hjust
                                                )#end aes
                                 , colour   = corr_opts$colour
                                 , family   = corr_opts$family
                                 , size     = corr_opts$fontsize
                                 )#end geom_segment
      #---~---


      #--- Plot the correlation title
      gg_now = gg_now + geom_text( data    = corr_opts$title
                                 , mapping = aes( x        = xlab
                                                , y        = ylab
                                                , label    = label
                                                )#end aes
                                 , hjust    = corr_opts$title$hjust
                                 , vjust    = corr_opts$title$vjust
                                 , angle    = corr_opts$title$angle
                                 , parse    = is.expression(corr_opts$name)
                                 , colour   = corr_opts$colour
                                 , family   = corr_opts$family
                                 , size     = corr_opts$fontsize
                                 , hjust    = 0.5
                                 , vjust    = 1.0
                                 )#end geom_segment
      #---~---



      #--- Plot the ideal point
      gg_now = gg_now + geom_star( data        = ideal
                                 , mapping     = aes_string( x = "x"
                                                           , y = "y"
                                                           )#end aes_string
                                 , colour      = ideal_opts$colour
                                 , fill        = ideal_opts$fill
                                 , starshape   = ideal_opts$starshape
                                 , size        = ideal_opts$size
                                 , starstroke  = ideal_opts$starstroke
                                 , show.legend = FALSE
                                 , inherit.aes = FALSE
                                 )#end geom_point
      #---~---



      #--- Plot points using colours and shapes.
      gg_now = gg_now + geom_point( data        = xsplit[[n]]
                                  , mapping     = aes_string( x      = "x"
                                                            , y      = "y"
                                                            , colour = aes_colour
                                                            , fill   = aes_colour
                                                            , shape  = aes_shape
                                                            )#end aes
                                  , size        = shape_opts$size
                                  , stroke      = shape_opts$stroke
                                  , show.legend =  (n == 1) && ((n_colour*n_shape) > 1)
                                  )#end geom_point
      #---~---


      #--- Define colour and shape settings.
      gg_now = gg_now + scale_colour_manual( name       = colour_opts$title
                                           , labels     = colour_opts$label
                                           , values     = colour_opts$level
                                           , aesthetics = c("colour","fill")
                                           )#end scale_colour_manual
      gg_now = gg_now + scale_shape_manual ( name       = shape_opts$title
                                           , labels     = shape_opts$label
                                           , values     = shape_opts$level
                                           )#end scale_shape_manual
      #---~---


      #--- Set cartesian coordinates to allow some annotation outside the range.
      gg_now = gg_now + coord_cartesian( xlim    = xlim_buff
                                       , ylim    = ylim_buff
                                       , expand  = FALSE
                                       , default = TRUE
                                       , clip    = "off"
                                       )#end coord_cartesian
      #---~---




      #--- Set axis names.
      margin_text  = margin(t=0.40,r=0.40,b=0.40,l=0.40,unit="char")
      margin_title = margin(t=0.00,r=0.00,b=0.00,l=0.00,unit="char")
      ticks_length = unit(-0.2,"char")
      if (pos_corr){
         axis_opts$x_colour = gamma_opts$colour
         axis_opts$y_colour = sigma_opts$colour
      }else{
         axis_opts$x_colour = sigma_opts$colour
         axis_opts$y_colour = sigma_opts$colour
      }#end if (pos_corr)
      gg_now = ( gg_now
               + theme( axis.text.x       = element_text( margin = margin_text
                                                        , colour = axis_opts$x_colour
                                                        , size   = axis_opts$fontsize
                                                        )#end element_text
                      , axis.text.y       = element_text( margin = margin_text
                                                        , colour = axis_opts$y_colour
                                                        , size   = axis_opts$fontsize
                                                        )#end element_text
                      , axis.ticks.length = ticks_length
                      , axis.title.x      = element_text( margin = margin_title
                                                        , colour = axis_opts$x_colour
                                                        , size   = axis_opts$fontsize
                                                        )#end element_text
                      , axis.title.y      = element_text( margin = margin_title
                                                        , colour = axis_opts$y_colour
                                                        , size   = axis_opts$fontsize
                                                        )#end element_text
                      )#end geom_point
               )#end gg_now
      #---~---


      #--- Copy data to list
      taylor[[n]] = gg_now
      #---~---
   }#end for (n in sequence(n_panel))
   #---~---

   #--- Final settings for the Taylor plot.
   taylor        = wrap_plots(taylor)
   margin_annot  = margin(t=0.35,r=0.35,b=0.35,l=0.35,unit="char")
   plot_title    = element_text( size = base_size     , margin = margin_annot)
   plot_subtitle = element_text( size = base_size *0.7, margin = margin_annot)
   if (extra_legend) taylor = taylor + guide_area()
   taylor = taylor + plot_layout(guides="collect",widths=c(1-f_legend,f_legend))
   taylor = taylor + plot_annotation( title    = main_title
                                    , subtitle = subtitle
                                    , theme    = theme( plot.title    = plot_title
                                                      , plot.subtitle = plot_subtitle
                                                      )#end theme
                                    )#end plot_annotation
   #---~---


   #--- Return object that can be plotted.
   return(taylor)
   #---~---
}#end gg_taylor
#==========================================================================================#
#==========================================================================================#
