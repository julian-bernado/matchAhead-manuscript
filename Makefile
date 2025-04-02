# Tell Make that these named targets aren't real files
.PHONY: all clean datasets models calipers predictions distances

# --------------------------------------------------------------------
# 1) Variables for Combinations
# --------------------------------------------------------------------
GRADES   := 3 4 5
SUBJECTS := glmath readng
YEARS    := 2019 2022   # Only used in step 1

# --------------------------------------------------------------------
# 2) STEP 1: DATASETS
#    We'll produce data/2019_3_glmath_df.csv and data/2022_3_glmath_df.csv, etc.
# --------------------------------------------------------------------

DATASETS := $(foreach y,$(YEARS), \
             $(foreach g,$(GRADES), \
             $(foreach s,$(SUBJECTS), \
             data/$(y)_$(g)_$(s)_df.csv )))

define build_dataset
data/$(1)_$(3)_$(2)_df.csv: scripts/1_create_datasets.R
	@echo "Creating dataset for subject=$(2), grade=$(3), year=$(1)"
	Rscript scripts/1_create_datasets.R $(3) $(2) $(1)
endef

$(foreach y,$(YEARS), \
  $(foreach s,$(SUBJECTS), \
    $(foreach g,$(GRADES), \
      $(eval $(call build_dataset,$(y),$(s),$(g))) \
    ) \
  ) \
)

datasets: $(DATASETS)

# --------------------------------------------------------------------
# 3) STEP 2: MODELS
# --------------------------------------------------------------------
MODELS := $(foreach grade,$(GRADES), \
           $(foreach subj,$(SUBJECTS), \
           models/$(grade)_$(subj)_model.rds))

define build_model
models/$(1)_$(2)_model.rds: data/2019_$(1)_$(2)_df.csv scripts/2_fit_models.R
	@echo "Creating model for grade=$(1), subject=$(2)"
	Rscript scripts/2_fit_models.R $(1) $(2)
endef

$(foreach grade,$(GRADES), \
  $(foreach subj,$(SUBJECTS), \
    $(eval $(call build_model,$(grade),$(subj))) \
  ) \
)

models: $(MODELS)

# --------------------------------------------------------------------
# 4) STEP 3: CALIPERS
# --------------------------------------------------------------------
CALIPERS := $(foreach grade,$(GRADES), \
             $(foreach subj,$(SUBJECTS), \
             calipers/$(grade)_$(subj)_caliper.rds))

define build_caliper
calipers/$(1)_$(2)_caliper.rds: models/$(1)_$(2)_model.rds scripts/3_calculate_calipers.R
	@echo "Creating caliper for subject=$(2), grade=$(1)"
	Rscript scripts/3_calculate_calipers.R $(1) $(2)
endef

$(foreach grade,$(GRADES), \
  $(foreach subj,$(SUBJECTS), \
    $(eval $(call build_caliper,$(grade),$(subj))) \
  ) \
)

calipers: $(CALIPERS)

# --------------------------------------------------------------------
# 5) STEP 4: PREDICTIONS
# --------------------------------------------------------------------
PREDICTIONS := $(foreach grade,$(GRADES), \
                $(foreach subj,$(SUBJECTS), \
                predictions/school/$(grade)_$(subj)_predictions.csv))

define build_prediction
predictions/school/$(1)_$(2)_predictions.csv: models/$(1)_$(2)_model.rds scripts/4_make_predictions.R
	@echo "Creating prediction for subject=$(2), grade=$(1)"
	Rscript scripts/4_make_predictions.R $(1) $(2)
endef

$(foreach grade,$(GRADES), \
  $(foreach subj,$(SUBJECTS), \
    $(eval $(call build_prediction,$(grade),$(subj))) \
  ) \
)

predictions: $(PREDICTIONS)

# --------------------------------------------------------------------
# 6) STEP 5: DISTANCES
# --------------------------------------------------------------------
distances:
	@for grade in $(GRADES); do \
		for subj in $(SUBJECTS); do \
			echo "Creating distances for subject=$$subj, grade=$$grade"; \
			Rscript scripts/5_get_distances.R $$grade $$subj; \
		done; \
	done

# --------------------------------------------------------------------
# 7) STEP 6: ISMs
# --------------------------------------------------------------------
isms:
	@for grade in $(GRADES); do \
		for subj in $(SUBJECTS); do \
			echo "Creating isms for subject=$$subj, grade=$$grade"; \
			Rscript scripts/6_form_isms.R $$grade $$subj; \
		done; \
	done

# --------------------------------------------------------------------
# 8) ALL TARGET
#    Build everything by typing "make" or "make all"
# --------------------------------------------------------------------
all: datasets models calipers predictions distances isms

# --------------------------------------------------------------------
# 9) CLEAN
# --------------------------------------------------------------------
clean:
	rm -f data/*.csv \
	      models/*.rds \
	      calipers/*.rds \
	      predictions/school/*.csv \
        predictions/student/*.csv \
	      distances/*.csv
