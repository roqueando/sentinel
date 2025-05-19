TARGET=sentinel
BUILDDIR=build
PRODUCTION_TARGET=$(BUILDDIR)/production/$(TARGET)

run: build
	@./$(BUILDDIR)/debug/$(TARGET)

production:
	@echo "building sentinel to production..."
	@go build -o $(PRODUCTION_TARGET) -ldflags="-s -w" ./cmd/sentinel

	@echo "build completed!"

build:
	@echo "building sentinel to development..."
	@go build -o $(BUILDDIR)/debug/$(TARGET) ./cmd/sentinel

	@echo "build completed!"
.PHONY: build run production
