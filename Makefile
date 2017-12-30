
check-system:
	bash ./bin/run-system-test gds/systems/govuk/publishing-e2e-tests-isolated.scm

debug-publishing-e2e-tests:
	bash ./guix-pre-inst-env guix system container -N --no-grafts --fallback gds/systems/govuk/publishing-e2e-tests.scm --share=../publishing-e2e-tests=/var/apps/publishing-e2e-tests
