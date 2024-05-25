describe('Universal Test', () => {
  let playerIDs = [];
  let giversFirst = [];
  let giversSecond = [];
  let giversFirstNames = [];
  let giversSecondNames = [];
  let allPlayerNames = [];

  before(() => {
    cy.task('getPlayersForCurrentWeek').then((result) => {
      playerIDs = result;

      // Separate playerIDs into giversFirst and giversSecond arrays
      giversFirst = playerIDs.filter((_, index) => index % 2 === 0);
      giversSecond = playerIDs.filter((_, index) => index % 2 !== 0);

      // Get player names for giversFirst and giversSecond
      giversFirstNames = giversFirst.map((id) => cy.task('getPlayerNameById', id));
      giversSecondNames = giversSecond.map((id) => cy.task('getPlayerNameById', id));

      // Get all player names
      cy.task('getAllPlayerNames').then((names) => {
        allPlayerNames = names;

        // Populate giversFirstNames and giversSecondNames
        giversFirstNames = giversFirst.map((id) => allPlayerNames[id] || '');
        giversSecondNames = giversSecond.map((id) => allPlayerNames[id] || '');
      });
    });
  });

  function giveAGift(giverId, receiverId, giverName, receiverName) {
    cy.visit('/')
    cy.get('.small_bundle_component') // Select the parent div
      .find('.small_bundle__button_buy') // Find the button within the parent div
      .contains('Collect') // Ensure it's the correct button with the text 'Collect'
      .click({ force: true }); // Click on the button

    cy.get('button.payment_modal__button.primary_ghost_button') // Select the button by its class
      .contains('For a friend') // Ensure it's the correct button with the text 'For a friend'
      .click(); // Click on the button

    cy.get('input#yourId').clear().type(giverId.toString());
    cy.get('input#friendId').clear().type(receiverId.toString());

    cy.get('svg.input_component__icon_spinner_placeholder.animate_rotate').should('not.exist');

    cy.get('div.input_component__label').contains("Friend's ID").closest('.input_component').find('.input_component__tip_error_text').should('not.have.text', '').then(($div) => {
      // Check if either of the conditions is met
      if ($div.text().includes(`Player name: ${receiverName}`)) {
        // If 'Player name: Player {giversReceiverId}' is visible, click 'Collect' button
        cy.get('button.payment_modal__button.main_button')
          .contains('Collect') // Ensure it's the correct button with the text 'Collect'
          .click();
        // Wait for the successful transaction message to appear
        cy.get('div.successful_payment__title').should('be.visible');
      } else if ($div.text().includes('The bundle is no longer available for this user today.')) {
        // If 'The bundle is no longer available for this user today.' is visible, log success and continue
        cy.log(`Success: The bundle is no longer available for Player ${giverId} and Player ${receiverId}.`);
      } else {
        // If neither message appears, fail the test
        throw new Error(`Neither message appeared for Player ${giverId} and Player ${receiverId}. Test failed.`);
      }
    });
  }

  function giveAGiftToThemselves(playerId, playerName) {
    cy.visit('/')
    cy.get('.small_bundle_component') // Select the parent div
      .find('.small_bundle__button_buy') // Find the button within the parent div
      .contains('Collect') // Ensure it's the correct button with the text 'Collect'
      .click({ force: true }); // Click on the button

    cy.get('input#yourId').clear().type(playerId.toString());

    cy.get('svg.input_component__icon_spinner_placeholder.animate_rotate').should('not.exist');

    cy.get('div.input_component__label').contains("Your ID").closest('.input_component').find('.input_component__tip_error_text').should('not.have.text', '').then(($div) => {
      // Check if either of the conditions is met
      if ($div.text().includes(`Player name: ${playerName}`)) {
        cy.get('button.payment_modal__button.main_button')
          .contains('Collect') // Ensure it's the correct button with the text 'Collect'
          .click();
        // Wait for the successful transaction message to appear
        cy.get('div.successful_payment__title').should('be.visible');
      } else if ($div.text().includes('The bundle is no longer available for this user today.')) {
        // If 'The bundle is no longer available for this user today.' is visible, log success and continue
        cy.log(`Success: The bundle is no longer available for Player ${playerId}.`);
      } else {
        // If neither message appears, fail the test
        throw new Error(`Neither message appeared for Player ${playerId}. Test failed.`);
      }
    });
  }

  it('gets player IDs from the database', () => {
    expect(playerIDs).to.have.lengthOf.at.least(1);
  });

  it('gets 2 arrays out of player IDs', () => {
    expect(Math.abs(giversFirst.length - giversSecond.length)).to.be.at.most(1);
  });

  it('gives free energy to all players', () => {
    // Ensure the lengths of giversFirst and giversSecond are equal
    const minLength = Math.min(giversFirst.length, giversSecond.length);

    // Loop through the giversFirst and giversSecond arrays
    for (let i = 0; i < minLength; i++) {
      const giversFirstId = giversFirst[i];
      const giversFirstName = giversFirstNames[i];
      const giversSecondId = giversSecond[i];
      const giversSecondName = giversSecondNames[i];

      giveAGift(giversFirstId, giversSecondId, giversFirstName, giversSecondName);
      giveAGift(giversSecondId, giversFirstId, giversSecondName, giversFirstName);
    }

    if (giversFirst.length > giversSecond.length) {
      const lastGiverFirstId = giversFirst[giversFirst.length - 1];
      const lastGiverFirstName = giversFirstNames[giversFirst.length - 1];
      giveAGiftToThemselves(lastGiverFirstId, lastGiverFirstName);
    }
  });
});
