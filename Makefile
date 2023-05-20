WORKFLOWS := .github/workflows/rhub.yaml

all : $(WORKFLOWS)

$(WORKFLOWS) : .github/workflows/%.yaml: inst/workflow/%.yaml
	perl -pe 's{r-hub/rhub2/actions/([\w-]+)\@v1}{./actions/$$1}g' $^ > $@

.PHONY: clean
clean:
	rm -f $(WORKFLOWS)
