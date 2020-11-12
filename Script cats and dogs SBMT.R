# loading the libraries

library(tidyverse)
library(gt)
library(showtext)
library(extrafont)
library(extrafontdb)
library(webshot)

### loading and organising the data

# uploading cat density
postcode_c <- read_csv("APHA0372-Cat_Density_Postcode_District.csv")

# uploading dog density
postcode_d <- read_csv("APHA0375-Dog_Density_Postcode_District.csv")


## merging the datasets into one density dataset of cats and dogs

postcode_dens <- postcode_c %>%
  inner_join(postcode_d, by = "PostcodeDistrict")

postcode_dens <- postcode_dens %>%
  rename(postcode_district = PostcodeDistrict,
         catpop = EstimatedCatPopulation,
         dogpop = EstimatedDogPopulation)

## uploading a dataset with areas, districts and human population

postcode_towns <- read_csv("Postcode districts.csv")

postcode_towns <- postcode_towns %>%
  select(Postcode, 'Town/Area', Region, Population, Households) %>%
  rename(postcode_district = Postcode,
         town = 'Town/Area',
         region = Region,
         humpop = Population,
         households = Households) 

# adding it to the previous dataframe

cats_dogs <- left_join(postcode_dens, postcode_towns, by = "postcode_district")

# filtering out postcode districts with no info on either of the living creatures

cats_dogs <- cats_dogs %>%
  filter(catpop > 0 & dogpop > 0 & humpop > 0 & !is.na(humpop))


# uploading the remaining data frames

max_cats <- read_csv("APHA0380-Cats_Per_Household_upper95.csv")
max_cats <- max_cats %>%
  rename(postcode_district = PostcodeDistrict)

max_dogs <- read_csv("APHA0383-Dogs_Per_Household_upper95.csv")
max_dogs <- max_dogs %>%
  rename(postcode_district = PostcodeDistrict)


# joining all this with the main dataset

cats_dogs <- cats_dogs %>%
  inner_join(max_cats, by = "postcode_district") %>%
  inner_join(max_dogs, by = "postcode_district") 


# checking which areas are top and bottom five by pets number

cats_dogs %>%
  group_by(region) %>%
  summarise(pets = sum(sum(catpop) + sum(dogpop)),
            pop = sum(humpop),
            cat_place = town[which.max(CatsPerHousehold_upper95)],
            cat_household = max(CatsPerHousehold_upper95),
            dog_town = town[which.max(DogsPerHousehold_upper95)],
            dog_household = max(DogsPerHousehold_upper95)) %>%
  ungroup() %>%
  slice_max(pets, n = 5)

# bottom 5

cats_dogs %>%
  group_by(region) %>%
  summarise(pets = sum(sum(catpop) + sum(dogpop)),
            pop = sum(humpop),
            cat_place = town[which.max(CatsPerHousehold_upper95)],
            cat_household = max(CatsPerHousehold_upper95),
            dog_place = town[which.max(DogsPerHousehold_upper95)],
            dog_household = max(DogsPerHousehold_upper95)) %>%
  ungroup() %>%
  slice_min(pets, n = 5) %>%
  arrange(desc(pets))

# this is a very primitive way to shorten the two long entries in the "town" variable
# unfortunately couldn't figure out how to do it with gsub or str_replace

cats_dogs$town <- as.factor(cats_dogs$town)

cats_dogs <- cats_dogs %>%
  mutate(town = fct_recode(town,
                           "Saxton, Stutton, Ulleskelf, etc." = "Saxton, Stutton, Ulleskelf, Church Fenton, Tadcaster, Toulston",
                           "Barlestone, Barton in the Beans, etc." = "Barlestone, Barton in the Beans, Bilstone, Cadeby, Carlton, Congerstone, Dadlington, Fenny Drayton, Higham on the Hill, Market Bosworth, Nailstone, Odstone, Osbaston, Shackerstone, Shenton, Stoke Golding, Sutton Cheney, Upton, Wellsborough"))




# making a plot function for the future table
# comparing the density per household of humans, dogs and cats

plot_comparison <- function(region, data) {
  
  data_p <- 
    cats_dogs %>% 
    filter(region == {{ region }}) %>% 
    group_by(region) %>%
    summarise(pets = sum(sum(catpop) + sum(dogpop)),
              human_d = sum(humpop)/sum(households),
              cat_d = sum(catpop)/sum(households),
              dog_d = sum(dogpop)/sum(households)) %>%
    ungroup() %>%
    gather("stat", "value", -c(region, pets)) %>%
    mutate(stat = factor(stat, levels = c("human_d", "dog_d", "cat_d"))) 
  
  plot <- 
    data_p %>% 
    ggplot(aes(x = region, y = value, fill = stat)) +
    geom_col(position = "dodge") +
    theme_void() +
    theme(
      legend.position = "none"
    ) +
    scale_fill_manual(values=c("#B62A3D", "#EDCB64","#B5966D")) +
    scale_y_continuous(breaks=c(0, 2.5))
    
  
  plot
  
}

### main parts of the table are tested and wrangled, time to assemble


# top 5
top_5 <- cats_dogs %>%
  group_by(region) %>%
  summarise(pets = sum(sum(catpop) + sum(dogpop)),
            pop = sum(humpop),
            cat_place = town[which.max(CatsPerHousehold_upper95)],
            cat_postcode = postcode_district[which.max(CatsPerHousehold_upper95)],
            cat_household = max(CatsPerHousehold_upper95),
            dog_place = town[which.max(DogsPerHousehold_upper95)],
            dog_household = max(DogsPerHousehold_upper95)) %>%
  ungroup() %>%
  slice_max(pets, n = 5)

# bottom 5

bottom_5 <- cats_dogs %>%
  group_by(region) %>%
  summarise(pets = sum(sum(catpop) + sum(dogpop)),
            pop = sum(humpop),
            cat_place = town[which.max(CatsPerHousehold_upper95)],
            cat_postcode = postcode_district[which.max(CatsPerHousehold_upper95)],
            cat_household = max(CatsPerHousehold_upper95),
            dog_place = town[which.max(DogsPerHousehold_upper95)],
            dog_household = max(DogsPerHousehold_upper95)) %>%
  ungroup() %>%
  slice_min(pets, n = 5) %>%
  arrange(desc(pets))


# main table

table <- rbind(top_5, bottom_5)

table_graphs <- table %>%
  mutate(plots = map(region, plot_comparison, data = cats_dogs)) %>%
  select(region, pop, pets, plots,cat_postcode, cat_place, cat_household, dog_household)

## gt table with plots

gt_table <- table_graphs %>%
  gt() %>%
  cols_align(
    align = "left",
    columns = 1
  ) %>%
  fmt_number(
    columns = vars(cat_household, dog_household),
    decimals = 1,
    use_seps = FALSE
  ) %>%
  fmt_number(
    columns = vars(pop, pets),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  tab_header(
    title = md("**It's raining cats and dogs**"),
    subtitle = html("<span style='color: black'>The <span style='color: #5299B7'><b>top</b></span> and</span>
                    <span style='color: #F39097'><b>bottom</b></span> five <b>UK</b> areas by pets population") 
  ) %>%
  tab_footnote(
    footnote = html("<span style='font-family:Muli'><i>Cats and dogs, populations are estimated</i></span>"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_spanner(
    label = html("<span style='font-family:Muli; font-size: 18px'><b>Postcodes with most pets per household</b></span>"),
    columns = vars(cat_postcode, cat_place, cat_household, dog_household)
  ) %>%
  tab_source_note(
    source_note = html("<span style='font-family:Muli'><b>Table:</b> @Ana_Voronkova // <b>Source:</b> data.gov.uk</span>")
  ) %>%
  cols_label(
    region = html(""),
    pop = html("Humans"),
    pets = html("Pets"),
    plots = html("<span style='color: #B62A3D;font-size: 18px'>Humans</span>, 
                 <span style='color: #EDCB64;font-size: 18px'>dogs</span> <span style = 'font-size:14px'>&</span> <span style='color: #B5966D;font-size: 18px'>cats</span><br>
                 <span style = 'font-size:14px'>per household</span>"),
    cat_postcode = html(local_image(filename = "letter.png",
                                    height = 35)),
    cat_place = html(local_image(filename = "postcode.png",
                                    height = 35)),
    cat_household = html(local_image(filename = "cat.png",
                                     height = 35)),
    dog_household = html(local_image(filename = "dog.png",
                                     height = 40))
  ) %>%
  tab_style(
    style = cell_text(size = px(18)),
    locations = cells_column_labels(everything())
  ) %>%
  cols_align(
    align = "center",
    columns = vars(pop, pets, cat_postcode, cat_place, cat_household, dog_household)
  ) %>%
  tab_style(
    cell_text(weight = "bold"),
    locations = cells_column_labels(vars(region, pop, pets, plots))
  ) %>%
  tab_style(
    style = cell_fill(color = "#C5DDE7"),
    locations = cells_body(
      columns = vars(region),
      rows = c(1:5))
  ) %>%
  tab_style(
    style = cell_fill(color = "#FDECED"),
    locations = cells_body(
      columns = vars(region),
      rows = c(6:10))
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("bottom"),
      color = "grey",
      weight = px(1),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = 5
    )
  ) %>%
  tab_style(
    style = cell_text(size = px(25), weight = "bold"),
    locations = cells_body(columns = vars(region),
                           rows = everything())
  ) %>%
  tab_style(
    style = cell_text(font = "Hashed Browns", size = px(60)),
    locations = cells_title("title")
  ) %>%
  tab_style(
    style = cell_text(font = "Muli", size = px(20)),
    locations = cells_title("subtitle")
  ) %>%
  tab_style(
    style = cell_text(font = "Muli"),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_text(font = "Muli"),
    locations = cells_column_labels(
      everything()
    )
  ) %>%
  tab_style(
    style = cell_text(font = "Muli"),
    locations = cells_column_spanners(
      everything()
    )
  ) %>%
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(30),
    column_labels.border.bottom.color = "grey",
    column_labels.border.bottom.width = px(2),
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table_body.border.bottom.color = "grey",
    table_body.border.bottom.width = px(2),
    table.border.top.color = "white"
  ) %>%
  text_transform(
    locations = cells_body(vars(plots)),
    fn = function(x) {
      map(table_graphs$plots, ggplot_image, height = px(80), aspect_ratio = 1.4)
    }
  )

# saving it as an image
  
gt_table %>%
  gt::gtsave(
    "catdog_table.png",
     path = "../R table competition"
  )


