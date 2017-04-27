#!/usr/bin/env groovy

properties([
  pipelineTriggers([
    upstream(upstreamProjects: 'integration-app-deploy'),
    cron('0 8-18 * * 1-5')
  ])
])

node("ci-agent-2") {
  try {
    stage("Checkout") {
      checkout([$class: 'GitSCM',
        branches: scm.branches,
        userRemoteConfigs: [[
          credentialsId: 'govuk-ci-ssh-key',
          url: "git@github.com:alphagov/govuk-guix.git"
        ]]
      ])
    }

    stage("govuk refresh") {
      withCredentials([
        string(
          credentialsId: 'github-token-govuk-ci',
          variable: 'GUIX_GITHUB_TOKEN'
        )
      ]) {
        sh "bash ./bin/govuk refresh --commit"
      }
    }

    stage("git push") {
      sshagent(['govuk-ci-ssh-key']) {
        sh("git push origin HEAD:master")
      }
    }
  } catch (e) {
    currentBuild.result = "FAILED"
    throw e
  }
}
