<apply template="base">
  <dfForm action="/boodschap">
    <dfChildErrorList ref="" />

    <dfLabel ref="boodschapper">Boodschapper: </dfLabel>
    <dfInputSelect ref="boodschapper" />
    <br>

    <dfLabel ref="bedrag">Bedrag: </dfLabel>
    <dfInputText ref="bedrag" />
    <br>


    <dfInputList ref="names">
      <dfListItem>
        <li itemAttrs>
        <dfInputText ref="naam" readonly />
        <dfInputCheckbox ref="eetMee"/>
        <br>
        </li>
      </dfListItem>
    </dfInputList>


    <dfInputSubmit value="Submit" />
  </dfForm>
</apply>