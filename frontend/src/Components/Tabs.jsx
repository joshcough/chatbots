import React from "react"
import { Tabs, Tab, Row, Col, Container, ListGroup, Card } from "react-bootstrap"

export const Main = ({items, defaultActiveKey}) =>
  <Tabs defaultActiveKey={defaultActiveKey}>
    {items.map(({title, inner}) =>
      <Tab eventKey={title} title={title}>
        {inner}
      </Tab>
    )}
  </Tabs>
